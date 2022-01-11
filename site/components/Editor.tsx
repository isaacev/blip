import { useCodeMirror } from "../hooks/useCodeMirror";
import {
  forwardRef,
  useEffect,
  useImperativeHandle,
  useRef,
  useState,
} from "react";
import type { EditorState } from "@codemirror/state";
import type { EditorView } from "@codemirror/view";
import { parser } from "../grammar/blip";
import { LRLanguage } from "@codemirror/language";
import { styleTags, tags as t } from "@codemirror/highlight";

const CACHE_KEY = "editor";

interface Ref {
  editor?: HTMLDivElement | null;
  state?: EditorState;
  view?: EditorView;
}

export interface Props {
  onChange?: (value: string) => void;
}

export const Editor = forwardRef<Ref, Props>((props, ref) => {
  const [value, setValue] = useState<string>();

  const editor = useRef<HTMLDivElement>(null);
  const { state, view, container, setContainer } = useCodeMirror({
    value: value ?? "",
    container: editor.current,
    language: LRLanguage.define({
      parser: parser.configure({
        props: [
          styleTags({
            "let match in print": t.keyword,
            Name: t.name,
            Number: t.number,
            String: t.string,
            Comment: t.comment,
          }),
        ],
      }),
      languageData: {
        commentTokens: { line: "--" },
      },
    }),
    onChange: (value) => {
      setValue(value);
      props.onChange?.(value);
    },
  });

  useImperativeHandle(ref, () => ({ editor: container, state, view }));

  useEffect(() => {
    setContainer(editor.current);
    return () => {
      if (view) {
        view.destroy();
      }
    };
  }, []);

  useEffect(() => {
    if (typeof value === "string") {
      localStorage.setItem(CACHE_KEY, value);
    } else {
      const stored = localStorage.getItem(CACHE_KEY);
      if (typeof stored === "string") {
        setValue(stored);
      }
    }
  }, [value, setValue]);

  return <div className="lg:absolute lg:inset-0" ref={editor}></div>;
});
