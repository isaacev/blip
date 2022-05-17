#!/usr/local/bin/python3

from os import listdir, remove
from os.path import isfile, isdir, join, splitext, exists
from sys import exit, stdout
from subprocess import run
from difflib import Differ
from argparse import ArgumentParser
from wasmtime import Store, Module, Instance, Func, FuncType, ValType

DEFAULT_TEST_DIR = "./tests"

COLOR_GREEN = "\033[32m"
COLOR_BRIGHT_RED = "\033[91m"
COLOR_WHITE = "\033[37m"
COLOR_RESET = "\033[0m"


EMPTY = [""]

def apply_ansi_color(str, color):
    if stdout.isatty():
        return f"{color}{str}{COLOR_RESET}"
    else:
        return str


class Test:
    def __init__(self, group_name, test_name, paths):
        self.name = "%s/%s" % (group_name, test_name)
        self.group_name = group_name
        self.test_name = test_name
        self.paths = paths

    def __repr__(self):
        return self.name

    def to_filepath_using_extension(self, ext):
        source_ext = ".blip"
        return self.paths[source_ext][: -len(source_ext)] + ext


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

                if result.wat_diff:
                    print()
                    print(f"  WAT  {legend}")
                    for line in result.wat_diff:
                        print("  ", end="")
                        line.print()

                if result.err_diff:
                    print()
                    print(f"  STDERR  {legend}")
                    for line in result.err_diff:
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
    # extension `.blip`. Other files with the extensions `.stderr` or `.wat`
    # will be considered part of the same test.
    for group_name, group in all_files.items():
        for test_name, files in group.items():
            # Only paths that are meaningful to the test runner
            test_paths = {}

            if ".blip" in files:
                test_paths[".blip"] = files[".blip"]

                if ".stderr" in files:
                    test_paths[".stderr"] = files[".stderr"]
                if ".wat" in files:
                    test_paths[".wat"] = files[".wat"]

                all_tests.append(Test(group_name, test_name, test_paths))

    # Sort tests by group_name/test_name for consistency during evaluation.
    all_tests.sort(key=lambda t: t.name)

    return all_tests


class PassedTest:
    def __init__(self, test):
        self.test = test


class FailedTest:
    def __init__(self, test, wat_found, err_found, wat_diff, err_diff):
        self.test = test
        self.wat_found = wat_found
        self.err_found = err_found
        self.wat_diff = wat_diff
        self.err_diff = err_diff


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
    if expected == EMPTY:
        expected = []
    if found == EMPTY:
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
        elif code == "? ":
            # ignore intraline differences
            continue

        diff_lines.append(DiffLine(mode, line))
    return diff_lines


def run_test(test, do_eval=False):
    src_path = test.paths[".blip"]
    output = run(
        ["./target/debug/client", src_path],
        text=True,
        capture_output=True,
    )
    wat_found = output.stdout.split("\n")
    err_found = output.stderr.split("\n")
    if output != EMPTY and do_eval:
        out_found = eval_test(test)
    else:
        out_found = EMPTY

    wat_expected = EMPTY
    if ".wat" in test.paths:
        with open(test.paths[".wat"], "r") as wat_file:
            wat_expected = wat_file.read().split("\n")

    err_expected = EMPTY
    if ".stderr" in test.paths:
        with open(test.paths[".stderr"], "r") as err_file:
            err_expected = err_file.read().split("\n")

    out_expected = EMPTY
    if ".stdout" in test.paths and do_eval:
        with open(test.paths[".stdout"], "r") as out_file:
            out_expected = out_file.read().split("\n")

    if wat_expected == wat_found and err_expected == err_found and out_expected == out_found:
        return PassedTest(test)
    else:
        wat_diff = diff(wat_expected, wat_found)
        err_diff = diff(err_expected, err_found)
        return FailedTest(test, wat_found, err_found, wat_diff, err_diff)


def eval_test(test):
    output = []
    def stdlib_print(val):
        output.append(val)

    store = Store()
    module = Module(store.engine, """
(module
  (import "" "print_i64" (func $print_i64 (param i64)))
  (func (export "main")
    i64.const 42
    call $print_i64))
""")
    instance = Instance(store, module, [
        Func(store, FuncType([ValType.i64()], []), stdlib_print),
    ])
    instance.exports(store)["main"](store)
    return PassedTest(test)


def bless(result):
    if isinstance(result, FailedTest):
        bless_extension(
            result.test.to_filepath_using_extension(".wat"), result.wat_found
        )
        bless_extension(
            result.test.to_filepath_using_extension(".stderr"), result.err_found
        )

        return PassedTest(result.test)

    return result


def bless_extension(filepath, blessed_lines):
    file_already_exists = exists(filepath)
    has_blessed_lines = blessed_lines != [] and blessed_lines != [""]

    if file_already_exists and not has_blessed_lines:
        # Handles the case:
        # - The file exists but is no longer needed for the test
        remove(filepath)
    elif has_blessed_lines:
        # Handles the cases:
        # - The file exists but the blessed contents are different
        # - The file doesn't exist yet but is now needed for the test
        with open(filepath, "w") as file:
            file.write("\n".join(blessed_lines))


def main():
    grammar = ArgumentParser(description="Run compiler tests")
    grammar.add_argument(
        "--bless",
        action="store_true",
        help="Update any output files for failing tests",
    )
    grammar.add_argument(
        "--eval",
        action="store_true",
        help="Evaluate generated code and check the result",
    )
    grammar.add_argument("filter", nargs="?", default="")
    args = grammar.parse_args()

    run(
        ["cargo", "build", "--bin", "client"],
        text=True,
        capture_output=True,
        check=True,
    )

    all_tests = find_tests()
    reporter = ShortReporter(len(all_tests))
    reporter.start()
    for test in all_tests:
        if test.name.startswith(args.filter):
            result = run_test(test, do_eval=args.eval)
            if args.bless and isinstance(result, FailedTest):
                result = bless(result)
            reporter.report(result)
        else:
            reporter.report(SkippedTest(test))
    reporter.finish()
    exit_code = 0 if len(reporter.failed_results) == 0 else 1
    exit(exit_code)


def say_hello():
    print("Hello from Python!")


if __name__ == "__main__":
    main()
