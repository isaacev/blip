import type { FC } from "react";

interface Props {
  title: string;
  children?: React.ReactNode;
}

export const Header: FC<Props> = (props) => {
  return (
    <header className="flex flex-row items-center gap-2">
      <h2 className="flex-1 text-xl">{props.title}</h2>
      {props.children}
    </header>
  );
};
