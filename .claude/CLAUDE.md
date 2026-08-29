# ggposter プロジェクト

R言語で学術ポスター(PDF、CJKフォント埋め込み対応)を作成するパッケージ。
レイアウトはRのリストまたはYAMLファイルで記述し、図・表・写真は別途Rオブジェクトとして渡す。

## check の生成物の後始末

- **`R CMD check` などで作られる `*.tar.gz` は，役割が終わったら削除する**．
  結果を確認し終えたら (CRAN へ出す場合は提出が済んだら) 消してよい．
  DESCRIPTION とソースから何度でも作り直せるため，残しておく理由がない．
- 同じ理由で，`*.Rcheck/` (check の作業ディレクトリ) も確認が済んだら消す．
- 補足: `*.tar.gz` を作るのは `R CMD build` / `devtools::build()` で，
  `devtools::check()` は既定で一時ディレクトリに作るためプロジェクト直下には残らない．
  プロジェクト直下に残るのは `R CMD build` を直接実行したときが多い．
  どちらの経路でできたものでも，見つけたら消す．

## 進捗状況

_最終更新: 2026-08-30 03:57 (x280-home)_

### ブランチ運用

**main 一本で作業する。develop は廃止した。**
一人開発で develop と main が並行して進んだことがなく、マージは常に fast-forward で
実質2本目の意味がなかったため。pkgdown サイトは main への push で毎回デプロイされる
(`.github/workflows/pkgdown.yaml`。PR ではビルド検証のみ)。
公開したくない変更はブランチではなくローカルに置いておく。

### 現在の状態

- 2026-08-30 03:57 (x280-home)
  **`inst/extdata/poster_sample.yml` の実在の共同研究者名・所属・科研費番号を架空化した**．
  acposter との比較調査中に発覚 (実際のポスターを逐語転記する vignette 節で使用)．
  あなた自身の氏名・所属 (Matsumura, Konan Women's Univ.) は残し，
  共同研究者2名の氏名・所属と KAKENHI 番号だけを架空のものに差し替えた (ユーザ確定)．

**このファイルをリポジトリ直下から `.claude/CLAUDE.md` へ移した** (2026-08-15 JST)．
プロジェクトの指示ファイルを `.claude/` にまとめる方針にしたため
(`comptea` も同じ日に同じ形にした)．

- `.gitignore` の `.claude/` を `.claude/*` ＋ `!.claude/CLAUDE.md` に変えた．
  ディレクトリごと除外していると，中のファイルを `!` で戻せないため．
  `.claude/settings.local.json` は除外のまま．
- `.Rbuildignore` の `^\.claude$` は元からあるので，`R CMD build` の対象外のまま．

以下は移動前の記録．

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
