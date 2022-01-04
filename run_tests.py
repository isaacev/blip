#!/usr/local/bin/python3

from os import listdir
from os.path import isfile, isdir, join, split, splitext
from random import getrandbits
from subprocess import run
from sys import argv
from difflib import Differ

DEFAULT_TEST_DIR = "./tests"

COLOR_GREEN = "\033[32m"
COLOR_BRIGHT_RED = "\033[91m"
COLOR_WHITE = "\033[37m"
COLOR_RESET = "\033[0m"


def apply_ansi_color(str, color):
    return f"{color}{str}{COLOR_RESET}"


class Test:
    def __init__(self, group_name, test_name, paths):
        self.name = "%s/%s" % (group_name, test_name)
        self.group_name = group_name
        self.test_name = test_name
        self.paths = paths

    def __repr__(self):
        return self.name


class Reporter:
    def __init__(self, total):
        self.total = total
        self.total_run = 0
        self.total_passed = 0
        self.total_failed = 0
        self.total_skipped = 0

    def start(self):
        pass

    def report_pass(self, result):
        self.total_run += 1
        self.total_passed += 1

    def report_fail(self, result):
        self.total_run += 1
        self.total_failed += 1

    def report_skip(self, result):
        self.total_run += 1
        self.total_skipped += 1

    def report(self, result):
        if isinstance(result, PassedTest):
            self.report_pass(result)
        elif isinstance(result, FailedTest):
            self.report_fail(result)
        elif isinstance(result, SkippedTest):
            self.report_skip(result)

    def finish(self):
        pass


class ShortReporter(Reporter):
    tests_per_line = 50

    def __init__(self, total):
        super().__init__(total)
        self.total_on_line = 0
        self.failed_results = []

    def is_line_finished(self, after_last_test=False):
        if (self.total_on_line % self.tests_per_line == 0) != after_last_test:
            line_padding = " " * (self.tests_per_line - self.total_on_line)
            num_width = len(str(self.total))
            progress = str(self.total_run).rjust(num_width, " ")
            print(line_padding + " " + progress + "/" + str(self.total))
            self.total_on_line = 0

    def start(self):
        super().start()
        print()
        print(f"found {self.total} tests")

    def report_pass(self, result):
        super().report_pass(result)
        print(apply_ansi_color("+", COLOR_GREEN), end="")
        self.total_on_line += 1
        self.is_line_finished()

    def report_fail(self, result):
        super().report_fail(result)
        print(apply_ansi_color("x", COLOR_BRIGHT_RED), end="")
        self.total_on_line += 1
        self.is_line_finished()
        self.failed_results.append(result)

    def report_skip(self, result):
        super().report_skip(result)
        print(apply_ansi_color(".", COLOR_WHITE), end="")
        self.total_on_line += 1
        self.is_line_finished()

    def finish(self):
        super().finish()
        self.is_line_finished(after_last_test=True)

        if self.failed_results:
            legend_expected = apply_ansi_color("-expected", COLOR_GREEN)
            legend_found = apply_ansi_color("+found", COLOR_BRIGHT_RED)
            legend = f"( {legend_expected} / {legend_found} )"
            ordinal = 1
            for result in self.failed_results:
                print()
                print(f"{ordinal}) {result.test.name}")
                ordinal += 1

                if result.stdout_diff:
                    print()
                    print(f"  STDOUT  {legend}")
                    for line in result.stdout_diff:
                        print("  ", end="")
                        line.print()

                if result.stderr_diff:
                    print()
                    print(f"  STDERR  {legend}")
                    for line in result.stderr_diff:
                        print("  ", end="")
                        line.print()

        print()
        print(f"passed  {self.total_passed}")
        print(f"failed  {self.total_failed}")
        print(f"skipped {self.total_skipped}")


def find_tests(root=DEFAULT_TEST_DIR):
    # Collect paths to files grouped by directory, base name, and extension.
    all_files = {}

    # Explore the ./tests directory. Find files with the same base name but
    # different extensions. When files share the same name and are in the same
    # tests directory, they are considered part of the same test.
    for group_name in listdir(root):
        dir_path = join(root, group_name)
        if isdir(dir_path):
            for file_name in listdir(dir_path):
                file_path = join(dir_path, file_name)
                if isfile(file_path):
                    [test_name, test_ext] = splitext(file_name)

                    if group_name not in all_files:
                        all_files[group_name] = {}

                    if test_name not in all_files[group_name]:
                        all_files[group_name][test_name] = {}

                    all_files[group_name][test_name][test_ext] = file_path

    # Create a list to collect finished Test objects.
    all_tests = []

    # For each list of files with the same base name but different file
    # extensions, treat the files as a single test if one of the files has the
    # extension `.blip`. Other files with the extensions `.stderr` or `.stdout`
    # will be considered part of the same test.
    for group_name, group in all_files.items():
        for test_name, files in group.items():
            # Only paths that are meaningful to the test runner
            test_paths = {}

            if ".blip" in files:
                test_paths[".blip"] = files[".blip"]

                if ".stderr" in files:
                    test_paths[".stderr"] = files[".stderr"]
                if ".stdout" in files:
                    test_paths[".stdout"] = files[".stdout"]

                all_tests.append(Test(group_name, test_name, test_paths))

    # Sort tests by group_name/test_name for consistency during evaluation.
    all_tests.sort(key=lambda t: t.name)

    return all_tests


class PassedTest:
    def __init__(self, test):
        self.test = test


class FailedTest:
    def __init__(self, test, stdout_diff, stderr_diff):
        self.test = test
        self.stdout_diff = stdout_diff
        self.stderr_diff = stderr_diff


class SkippedTest:
    def __init__(self, test):
        self.test = test


class DiffLine:
    MODE_SAME = 0
    MODE_FOUND_NOT_EXPECTED = 1
    MODE_EXPECTED_NOT_FOUND = 2

    def __init__(self, mode, line):
        self.mode = mode
        self.line = line

    def print(self):
        if self.mode == DiffLine.MODE_SAME:
            print(apply_ansi_color(f". {self.line}", COLOR_WHITE))
        elif self.mode == DiffLine.MODE_FOUND_NOT_EXPECTED:
            print(apply_ansi_color(f"+ {self.line}", COLOR_BRIGHT_RED))
        elif self.mode == DiffLine.MODE_EXPECTED_NOT_FOUND:
            print(apply_ansi_color(f"- {self.line}", COLOR_GREEN))


differ = Differ()


def diff(expected, found):
    if expected == [""]:
        expected = []
    if found == [""]:
        found = []

    if expected == found:
        return None

    diff_lines = []
    for differ_line in differ.compare(expected, found):
        mode = DiffLine.MODE_SAME
        code = differ_line[0:2]
        line = differ_line[2:]

        if code == "+ ":
            mode = DiffLine.MODE_FOUND_NOT_EXPECTED
        elif code == "- ":
            mode = DiffLine.MODE_EXPECTED_NOT_FOUND

        diff_lines.append(DiffLine(mode, line))
    return diff_lines


def run_test(test):
    src_path = test.paths[".blip"]
    output = run(
        ["./target/debug/client", src_path],
        text=True,
        capture_output=True,
    )
    stdout_found = output.stdout.split("\n")
    stderr_found = output.stderr.split("\n")

    stdout_expected = ['']
    if ".stdout" in test.paths:
        with open(test.paths[".stdout"], "r") as stdout_file:
            stdout_expected = stdout_file.read().split("\n")

    stderr_expected = ['']
    if ".stderr" in test.paths:
        with open(test.paths[".stderr"], "r") as stderr_file:
            stderr_expected = stderr_file.read().split("\n")

    if stdout_expected == stdout_found and stderr_expected == stderr_found:
        return PassedTest(test)
    else:
        stdout_diff = diff(stdout_expected, stdout_found)
        stderr_diff = diff(stderr_expected, stderr_found)
        return FailedTest(test, stdout_diff, stderr_diff)


def main():
    run(["cargo", "build", "--bin", "client"], text=True, capture_output=True, check=True)

    prefix = argv[1] if len(argv) > 1 else ""
    all_tests = find_tests()
    reporter = ShortReporter(len(all_tests))
    reporter.start()
    for test in all_tests:
        if test.name.startswith(prefix):
            result = run_test(test)
            reporter.report(result)
        else:
            reporter.report(SkippedTest(test))
    reporter.finish()


if __name__ == "__main__":
    main()
