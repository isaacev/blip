import { useState } from "react";
import { Left } from "./Left";
import { OutputState, Right } from "./Right";

export const Workspace = () => {
  const [state, _setState] = useState<OutputState>({ name: "idle" });

  return (
    <div className="lg:fixed lg:inset-0 grid lg:grid-cols-2 max-w-[1600px] mx-auto">
      <Left />
      <Right state={state} />
    </div>
  );
};
