import { FunctionComponent } from "react";
import { render } from "react-dom";
import CodeMirror, { EditorView } from "@uiw/react-codemirror";
import {
  CompiledModule,
  Compiler,
  CompilerError,
  CompilerResult,
  SnippetSection,
  SourceFile,
} from "./compiler";
import "./styles.css";
import { StrictMode, useEffect, useState } from "react";

const fontFamily = `"Dank Mono", monospace`;
const fontSize = "20px";
const theme = EditorView.theme({
  "&": { height: "100%", width: "100%" },
  "&.cm-editor.cm-focused": { outline: "none" },
  ".cm-gutters": {
    paddingLeft: "8px",
    fontFamily,
    fontSize,
    borderRight: "none",
  },
  ".cm-content": {
    fontFamily,
    fontSize,
    paddingTop: "8px",
    paddingBottom: "8px",
  },
  ".cm-line": { paddingLeft: "8px", paddingRight: "8px" },
});

type AppState =
  | { tag: "idle" }
  | { tag: "start"; file: SourceFile }
  | { tag: "working"; file: SourceFile }
  | { tag: "finished"; result: CompilerResult };

type OnStart = (file: SourceFile) => void;

const Editor = (props: { state: AppState; onStart: OnStart }) => {
  const [contents, setContents] = useState<string | null>(null);

  const onClick = () => {
    if (contents !== null) {
      props.onStart({ filename: "example", contents });
    }
  };

  const isWorking = ["start", "working"].includes(props.state.tag);

  useEffect(() => {
    if (contents === null) {
      const storedContents = localStorage.getItem("contents");
      if (typeof storedContents === "string") {
        setContents(storedContents);
      } else {
        setContents(
          `let x = 123 in\nlet y = 456 + x in\nlet z = y + y in\nx + y + z`
        );
      }
    } else {
      localStorage.setItem("contents", contents);
    }
  }, [contents, setContents]);

  return (
    <div className="p-4 flex flex-col gap-4">
      <header className="flex flex-row items-center">
        <h2 className="flex-1 text-xl leading-none">Editor</h2>
        <button
          className="bg-blue-500 text-white px-4 py-1 rounded cursor-pointer active:bg-blue-600 disabled:bg-gray-300 disabled:cursor-wait"
          disabled={isWorking}
          onClick={onClick}
        >
          Compile
        </button>
      </header>
      <div className="flex-1 relative">
        <CodeMirror
          value={contents ?? ""}
          onChange={setContents}
          theme={theme}
        />
      </div>
    </div>
  );
};

const RawText: FunctionComponent = (props) => (
  <pre className="px-4 py-2 bg-gray-100 rounded border">{props.children}</pre>
);

const IdleOutput = () => (
  <p className="text-gray-500">
    <em>Nothing has been compiled yet.</em>
  </p>
);

const WorkingOutput = () => (
  <p className="text-gray-500">
    <em>Compiling&hellip;</em>
  </p>
);

const WAT_SPEC = `https://webassembly.github.io/spec/core/text/index.html`;
const WAT_DOC = `https://developer.mozilla.org/en-US/docs/WebAssembly/Understanding_the_text_format`;

const OkOutput = ({ module }: { module: CompiledModule }) => (
  <>
    <p>The program was successfully compiled into WebAssembly:</p>
    <RawText>{module}</RawText>

    <details>
      <summary>What is WebAssembly Text Format (WAT)?</summary>

      <p className="pb-4">
        While WebAssembly is actually a binary format, the compiled program is
        displayed as{" "}
        <a target="_blank" href={WAT_SPEC}>
          WebAssembly Text Format
        </a>
        .
      </p>

      <figure className="border-l-4 border-gray-200 pl-4 py-2">
        <blockquote cite={WAT_DOC}>
          To enable WebAssembly to be read and edited by humans, there is a
          textual representation of the wasm binary format. This is an
          intermediate form designed to be exposed in text editors, browser
          developer tools, etc.
        </blockquote>
        <figcaption className="mt-2">
          —{" "}
          <a target="_blank" href={WAT_DOC}>
            MDN
          </a>
        </figcaption>
      </figure>
    </details>
  </>
);

const ErrSnippet = (props: { snippet: SnippetSection }) => {
  const { lines, callout } = props.snippet;
  if (lines.length === 0) {
    return (
      <RawText>
        <em>Empty snippet</em>
      </RawText>
    );
  }

  const maxLineNum = Math.max(
    ...lines.map((l) => (l.type === "source" ? l.line_num : 0))
  );
  const gutterWidth = maxLineNum.toString().length;

  const l = lines.map((line, index) => {
    const prefix = index === 0 ? "" : "\n";
    if (line.type === "source") {
      const gutter = line.line_num.toString().padStart(gutterWidth);
      return `${prefix}${gutter} | ${line.regions.map((r) => r.text).join("")}`;
    } else {
      const gutter = "".padStart(gutterWidth);
      const offset = " ".repeat(line.start_column - 1);
      const underline = callout.underline.repeat(line.width);
      return `${prefix}${gutter} | ${offset}${underline}`;
    }
  });

  return (
    <>
      <p>
        The issue occurs at line {callout.start_line}, column{" "}
        {callout.start_column}:
      </p>
      <RawText>
        {l.map((line, index) => {
          return <span key={index}>{line}</span>;
        })}
      </RawText>
    </>
  );
};

const ErrSection = (props: { section: CompilerError["sections"][number] }) => {
  switch (props.section.type) {
    case "snippet":
      return <ErrSnippet snippet={props.section} />;
    default:
      return null;
  }
};

const ErrOutput = ({ err }: { err: CompilerError }) => (
  <div className="p-4 flex-col gap-4 bg-red-100 flex rounded">
    <h3 className="text-2xl leading-none">Error: {err.title}</h3>
    {err.sections.map((section, index) => (
      <ErrSection key={index} section={section} />
    ))}
  </div>
);

const OutputBody = ({ state }: { state: AppState }) => {
  if (state.tag === "idle") {
    return <IdleOutput />;
  } else if (state.tag === "start" || state.tag === "working") {
    return <WorkingOutput />;
  } else if (state.result.tag === "ok") {
    return <OkOutput module={state.result.module} />;
  } else {
    return <ErrOutput err={state.result.err} />;
  }
};

const Output = (props: { state: AppState; onClear: () => void }) => {
  return (
    <div className="p-4 flex flex-col gap-4 overflow-y-scroll">
      <header className="flex items-center">
        <h2 className="flex-1 text-xl leading-none">Output</h2>
        <button
          className="bg-red-500 text-white px-4 py-1 rounded cursor-pointer active:bg-red-600"
          onClick={props.onClear}
        >
          Clear
        </button>
      </header>
      <OutputBody state={props.state} />
    </div>
  );
};

const App = () => {
  const [state, setState] = useState<AppState>({ tag: "idle" });

  const onStart = (file: SourceFile) => {
    if (state.tag === "working") {
      // If a compilation is already in-progress, ignore new compilation requests.
      return;
    }

    setState({ tag: "start", file });
  };

  const onClear = () => {
    if (state.tag !== "working") {
      setState({ tag: "idle" });
    }
  };

  useEffect(() => {
    if (state.tag === "start") {
      console.log("start compilation");
      setState({ tag: "working", file: state.file });
      Compiler.compile(state.file).then((result) => {
        console.log("finished compilation", result);
        setState({ tag: "finished", result });
      });
    }
  }, [state]);

  return (
    <>
      <h1 className="sr-only">Blip language editor</h1>
      <div className="grid grid-cols-2 gap-8 fixed inset-0">
        <Editor state={state} onStart={onStart} />
        <Output state={state} onClear={onClear} />
      </div>
    </>
  );
};

render(
  <StrictMode>
    <App />
  </StrictMode>,
  document.getElementById("app")
);
