const unloadedPkg = import("../pkg");

export interface SourceFile {
  filename: string;
  contents: string;
}

export type CompiledModule = string;

interface SnippetRegion {
  text: string;
  kind: string | null;
}

type SnippetLine =
  | { type: "source"; line_num: number; regions: SnippetRegion[] }
  | { type: "callout"; start_column: number; width: number };

export interface SnippetSection {
  type: "snippet";
  filename: string;
  lines: Array<SnippetLine>;
  callout: {
    start_line: number;
    start_column: number;
    end_line: number;
    end_column: number;
    style: string;
    underline: string;
    message: string | null;
  };
}

export interface CompilerError {
  title: string;
  sections: Array<SnippetSection>;
}

export type CompilerResult =
  | { tag: "ok"; module: CompiledModule }
  | { tag: "err"; err: CompilerError };

type RawRustValue = { Ok: CompiledModule } | { Err: unknown };

export class Compiler {
  static async compile(file: SourceFile): Promise<CompilerResult> {
    const pkg = await unloadedPkg;
    const result: RawRustValue = pkg.compile_for_js(file.contents);

    if (typeof (result as any).Ok === "string") {
      return { tag: "ok", module: (result as any).Ok };
    } else {
      console.error(JSON.stringify(result));
      return { tag: "err", err: (result as any).Err };
    }
  }
}
