import { useEffect, useState } from "react";
import { EditorState } from "@codemirror/state";
import {
  drawSelection,
  EditorView,
  highlightActiveLine,
  keymap,
  ViewUpdate,
} from "@codemirror/view";
import { highlightActiveLineGutter, lineNumbers } from "@codemirror/gutter";
import { history } from "@codemirror/history";
import type { LRLanguage } from "@codemirror/language";
import { defaultHighlightStyle } from "@codemirror/highlight";
import { commentKeymap } from "@codemirror/comment";
import { closeBrackets } from "@codemirror/closebrackets";
import { indentWithTab } from "@codemirror/commands";

interface Props {
  value?: string;
  onChange: (doc: string) => void;
  container?: HTMLDivElement | null;
  language: LRLanguage;
}

export function useCodeMirror(props: Props) {
  const { value, onChange } = props;
  const [container, setContainer] = useState(props.container);
  const [view, setView] = useState<EditorView>();
  const [state, setState] = useState<EditorState>();

  const updateListener = EditorView.updateListener.of((vu: ViewUpdate) => {
    if (vu.docChanged) {
      onChange(vu.state.doc.toString());
    }
  });

  const fontFamily = `'Dank Mono', monospace`;
  const extensions = [
    EditorView.theme({
      "&": { height: "100%" },
      ".cm-gutters": { "font-size": "20px", "font-family": fontFamily },
      ".cm-content": { "font-size": "20px", "font-family": fontFamily },
    }),
    updateListener,
    lineNumbers(),
    history(),
    highlightActiveLineGutter(),
    drawSelection(),
    highlightActiveLine(),
    defaultHighlightStyle.fallback,
    closeBrackets(),
    props.language,
    keymap.of([...commentKeymap, indentWithTab]),
  ];

  useEffect(() => {
    if (container && !state) {
      const stateCurrent = EditorState.create({ doc: value, extensions });
      setState(stateCurrent);

      if (!view) {
        const viewCurrent = new EditorView({
          state: stateCurrent,
          parent: container as any,
        });
        setView(viewCurrent);
      }
    }
  }, [container, state]);

  useEffect(() => {
    return () => {
      if (view) {
        view.destroy();
      }
    };
  }, [view]);

  useEffect(() => {
    if (view) {
      const currentValue = view.state.doc.toString();
      if (value !== currentValue) {
        view.dispatch({
          changes: { from: 0, to: currentValue.length, insert: value || "" },
        });
      }
    }
  }, [value, view]);

  return { state, setState, view, setView, container, setContainer };
}
