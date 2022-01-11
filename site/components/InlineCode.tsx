import type { FunctionComponent } from "react";

export const InlineCode: FunctionComponent = (props) => {
  return (
    <code className="p-1 rounded-sm bg-gray-100 font-mono whitespace-pre">
      {props.children}
    </code>
  );
};
