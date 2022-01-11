import { InlineCode } from "./InlineCode";

export interface Props {
  filename: string;
  origin: number;
  lines: Array<{ num: number | null; text: string }>;
}

type Line = Props["lines"][number];

const LineNumber = ({ num }: { num: Line["num"] }) => {
  if (num === null) {
    return <span className="block">&nbsp;</span>;
  }

  return <span className="block text-gray-400 text-right">{num}</span>;
};

const LineText = ({ text }: { text: Line["text"] }) => {
  return <span className="block">{text}</span>;
};

export const Snippet = (props: Props) => {
  return (
    <section>
      <header>
        <span>
          Error in <InlineCode>{props.filename}</InlineCode> starting on line{" "}
          {props.origin}
        </span>
      </header>
      <div className="mt-2 bg-gray-50 border rounded font-mono text-lg">
        <div className="p-3 grid grid-cols-[auto,1fr]">
          <div className="mr-3">
            {props.lines.map(({ num }, idx) => (
              <LineNumber key={idx} num={num} />
            ))}
          </div>
          <pre className="overflow-auto overflow-y-hidden">
            <code>
              {props.lines.map(({ text }, idx) => (
                <LineText key={idx} text={text} />
              ))}
            </code>
          </pre>
        </div>
      </div>
    </section>
  );
};
