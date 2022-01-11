import { StrictMode } from "react";
import { render } from "react-dom";
import { Workspace } from "./components/Workspace";

const root = document.getElementById("root");
render(
  <StrictMode>
    <Workspace />
  </StrictMode>,
  root
);
