import { useEffect, useState } from "react";
import { render } from "react-dom";

const blip = import("../pkg");

const compile = async (prog: string): Promise<any> => {
  const { compile_for_js } = await blip;
  return compile_for_js(prog);
};

const App = () => {
  const [editor, setEditor] = useState("2 + 2");
  const [prog, setProg] = useState<any>(null);
  return (
    <>
      <textarea onChange={(e) => setEditor(e.target.value)}>{editor}</textarea>
      <button onClick={() => setProg(editor)}>Compile</button>
      {prog !== null && <Result prog={prog} />}
    </>
  );
};

const Result = (props: { prog: string }) => {
  const [output, setOutput] = useState<any>(null);

  useEffect(() => {
    compile(props.prog).then((output) => {
      if (typeof output?.Ok === "string") {
        return setOutput(output.Ok);
      }

      return setOutput(JSON.stringify(output, null, "  "));
    });
  });

  return <pre>{output}</pre>;
};

render(<App />, document.getElementById("app"));
