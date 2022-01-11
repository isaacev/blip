import { PrimaryButton, SecondaryButton } from "./Button";
import { Editor } from "./Editor";
import { Header } from "./Header";

interface Props {
  onCompile?: (program: string) => void;
}

export const Left = (props: Props) => {
  return (
    <section className="flex flex-col gap-4 p-4">
      <Header title="Editor">
        <SecondaryButton>Reset</SecondaryButton>
        <PrimaryButton>Compile</PrimaryButton>
      </Header>
      <div className="flex-1 lg:relative">
        <Editor />
      </div>
    </section>
  );
};
