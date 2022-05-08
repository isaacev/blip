import type { FC } from "react";

interface Props {
  children?: React.ReactNode;
}

export const InlineCode: FC<Props> = (props) => {
  return (
    <code className="p-1 rounded-sm bg-gray-100 font-mono whitespace-pre">
      {props.children}
    </code>
  );
};
