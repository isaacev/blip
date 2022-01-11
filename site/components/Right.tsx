import { Header } from "./Header";
import { InlineCode } from "./InlineCode";
import { Snippet, Props as SnippetProps } from "./Snippet";

export type OutputState =
  | { name: "idle" }
  | { name: "waiting" }
  | { name: "success" }
  | { name: "error" };

interface Props {
  state: OutputState;
}

export const Right = (_props: Props) => {
  const lines: SnippetProps["lines"] = [
    { num: 8, text: "let x = 123 in" },
    { num: 9, text: "let y = x + 456 in" },
    { num: null, text: "        ^ error occured here" },
    { num: 10, text: "x + y" },
  ];

  return (
    <section className="flex flex-col gap-4 p-4">
      <Header title="Output" />
      <div className="flex-1 flex flex-col gap-4">
        <p>
          Veniam reprehenderit cillum voluptate elit officia non do veniam
          officia dolor laboris proident anim laborum. Magna et ea laboris ut
          ipsum. <InlineCode>Ea duis mollit anim</InlineCode> Lorem. Sit et
          voluptate magna ipsum eiusmod eiusmod ullamco cupidatat ad quis non
          dolore cupidatat. Proident consectetur proident enim duis. Elit magna
          aute aute irure aute. Consectetur ipsum fugiat reprehenderit culpa
          ullamco est est occaecat voluptate elit nisi enim.
        </p>
        <Snippet filename="example" origin={9} lines={lines} />
      </div>
    </section>
  );
};
