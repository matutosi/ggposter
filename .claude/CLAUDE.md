# ggposter プロジェクト

R言語で学術ポスター(PDF、CJKフォント埋め込み対応)を作成するパッケージ。
レイアウトはRのリストまたはYAMLファイルで記述し、図・表・写真は別途Rオブジェクトとして渡す。

## 進捗状況

_最終更新: 2026-08-15_

### ブランチ運用

**main 一本で作業する。develop は廃止した。**
一人開発で develop と main が並行して進んだことがなく、マージは常に fast-forward で
実質2本目の意味がなかったため。pkgdown サイトは main への push で毎回デプロイされる
(`.github/workflows/pkgdown.yaml`。PR ではビルド検証のみ)。
公開したくない変更はブランチではなくローカルに置いておく。

### 現在の状態

README(英語・日本語)と `R/` の API、および同梱 YAML (`inst/extdata/`) の整合を確認・修正し、
README.md と READMEjp.md を `rmarkdown::render()` で再生成した。

- `R/*.R` の公開 API(`poster()` / `render_poster()` / `theme_green()` / body の
  text・table・figure・image と各引数)は README の記述と一致していた。
- ズレていたのは README と同梱 YAML `inst/extdata/poster_sample_howto.yml`。
  README は「p_howto_yml は p_howto と同一」と書いているが、c0d0360 で README 側だけ
  カード順(title/layout を先頭へ)と図の高さ(55→100、90→110)を変更し、YAML を
  追随させていなかった。YAML を README 側に合わせて更新。
- README/READMEjp 内でカード上に表示していた `height: 80` などの説明文が実際の値
  (100 / 110)と食い違っていたので修正。左列カードの並び順を説明する本文も、
  実際の並び(title・layout が先頭)に合わせて書き換え。
- READMEjp.Rmd の冒頭コメントを「READMEjp.md is generated from READMEjp.Rmd」に修正。
- README.md / READMEjp.md を再生成(旧 README.md は古い pandoc 由来で R コードが
  字下げブロックだったが、現行 pandoc 3.8.2 では ``` r のフェンスブロックになる)。
- 検証: `knitr::purl()` で README/READMEjp の R 仕様を取り出し、`read_poster_yaml()`
  で読んだ同梱 YAML と `all.equal()` で比較して一致を確認。

### 直近のコミット履歴(develop ブランチ)

- ee79923 日本語版 README(READMEjp.Rmd)を追加
- 488b6f6 図のキャプションを図の下に箇条書きとして表示できるように対応
- c0d0360 howto レイアウト仕様のカンマ抜けを修正し README を再knit
- 02db6e0 チュートリアルポスターでタイトルバンドとレイアウト設定を解説
- cd1e42c テキストカードのインデント保持と列をまたいだ行揃えを追加
- ecb0d76 「How to Make an Academic Poster」チュートリアルに YAML 仕様の中央列を追加
- 0440ca7 README.md を再knitし、誤って評価されていたインストールチャンクを修正
- bbedfd9 「How to Make an Academic Poster」チュートリアルポスターのレイアウトを改訂

### 未コミットの変更

- `README.Rmd` / `READMEjp.Rmd`: 表示上の高さの値・本文のカード順・生成元コメントを修正。
- `README.md`(再生成)、`READMEjp.md`(新規)、`man/figures/README-howto-poster.png`(再生成)。
- `inst/extdata/poster_sample_howto.yml`: README の仕様に合わせて更新。
- `.gitignore`: `poster.pdf`・`*.html`(github_document のプレビュー)・`.claude/` を
  追跡対象外に追加。`CLAUDE.md` は追跡する。

### 次にやること候補

- 特になし。
