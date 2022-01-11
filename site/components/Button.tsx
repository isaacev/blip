import type { FunctionComponent } from "react";

interface Props {
  disabled?: boolean;
}

export const PrimaryButton: FunctionComponent<Props> = (props) => {
  return (
    <button
      className="bg-blue-500 text-white px-4 py-1 rounded cursor-pointer active:bg-blue-600 disabled:bg-gray-300 disabled:cursor-wait focus:ring focus:outline-none"
      disabled={props.disabled}
    >
      {props.children}
    </button>
  );
};

export const SecondaryButton: FunctionComponent<Props> = (props) => {
  return (
    <button
      className="bg-gray-500 text-white px-4 py-1 rounded cursor-pointer active:bg-gray-600 disabled:bg-gray-300 disabled:cursor-wait focus:ring focus:outline-none"
      disabled={props.disabled}
    >
      {props.children}
    </button>
  );
};
