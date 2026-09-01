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

_最終更新: 2026-08-31 15:20 (MATUTOSI_DP)_

### ブランチ運用

**main 一本で作業する。develop は廃止した。**
一人開発で develop と main が並行して進んだことがなく、マージは常に fast-forward で
実質2本目の意味がなかったため。pkgdown サイトは main への push で毎回デプロイされる
(`.github/workflows/pkgdown.yaml`。PR ではビルド検証のみ)。
公開したくない変更はブランチではなくローカルに置いておく。

### 現在の状態

- 2026-09-02 02:53 (このセッション，x280-home)
  **バグ・リファクタリング・テストを点検し，バグ7件を直した** (ユーザ指示の 2)．
  - **実害2件**: (1) `width` だけを与えた図が `height: "auto"` のカードで
    **高さ0**になり，ヘッダと余白だけの箱に潰れていた (`poster_card()` が
    `measured_size` の**有無**だけを見ており，`poster_fix_size()` は寸法を
    **片方ずつ**しか入れないため)．(2) `columns: 4` と書いても空の列が
    落とされ，**3列**で組まれていた (姉妹ツールと配置が食い違う)．
  - **静かに間違う4件**: `grid:` の `w: 0`・`h: -1`・`x: 0.9` が素通り
    (`w: 0` は重なり検査もすり抜け，`r < l` の不正な gtable になっていた)／
    **`theme$cjk_family` がどこからも使われていなかった** (別名・promote・
    print はあるのに `gpar()` に届かず，和文も `base_family` で描いていた)／
    空入力の意味不明なエラー／YAML 1.1 で真偽値になるキー (`on:` など)．
  - **7件目は `document()` で露見した既存のバグ**．`read_poster_yaml()` の
    roxygen ブロックが `restore_y_key()` の上に取り残されており，
    **次に `document()` を回すと `read_poster_yaml()` が export から外れる**
    状態だった (`man/`・`NAMESPACE` が 2026-08-31 以降古いままだった)．
  - `cjk_family` は**文字列ごとに切り替える** (CJK を含む行だけ `cjk_family`，
    ほかは `base_family`)．grid は1つの `gpar()` の中で書体を切り替えられないため．
  - **検証**: 回帰テスト31件を追加 (153 → 184件，全通過)．
    `R CMD check` **0/0/0**．見本4本とも A1・1ページで変わらず組めた．

- 2026-08-31 15:20 (このセッション，MATUTOSI_DP) その3
  **README (英・日) を acposter・qtposter に揃えた** (ユーザ指示)．
  - **長いコードを別ファイルへ出した**．`README.Rmd` は 696 → 約 290 行．
    例のポスターの spec と図表 (140 行) を **`inst/extdata/readme_example.R`** に移し
    (`readme_example_parts()`/`_objects()`/`_spec()`/`_poster()`)，
    「カードの種類の一巡り」の 270 行のチャンクは**丸ごと落とした**
    (同じ内容が `poster_sample_howto.yml` にあり，`sample_objects()` で組める)．
  - **見本4本の縮小画像を入れた** (acposter と同じ，クリックで spec へ飛ぶ2×2 の表)．
    **置き場所は `man/figures/` で，姉妹ツールの `previews/` とは違う**．
    pkgdown が参照サイトへ複写するのがこのディレクトリだけのため．
    作り方は `pdftoppm -r 26` (A1 が 600px 幅ほど．A0 を `-r 18` で起こす姉妹ツールと同じ大きさ)．
  - **姉妹ツールとの行き来にキーの対応表を足した** (意味・正・受ける別名)．
    acposter・qtposter の README と同じ形．`cols` は元から通ることを確認した．
  - 使わなくなった `man/figures/README-howto-poster.png` は消した．
  - **検証**: 両 README を knit．テスト153件通過．

- 2026-08-31 11:35 (このセッション，MATUTOSI_DP) その2
  **CI を足し，README にライセンス節を置いた** (ユーザ指示)．
  - `.github/workflows/test.yml`: **`R CMD check` (Ubuntu・macOS)** と，
    **見本4本のビルド**の2ジョブ．見本は `pdftotext` で中身の文字列まで確かめ，
    PDF を成果物として上げる (acposter・qtposter と同じ考え方)．
  - ローカルの `R CMD check` は **0 errors / 0 warnings / 1 note**．
    note は「隠しファイル `.git`」で，**worktree で検査したため** (worktree では
    `.git` がファイルとして残る)．CI の checkout では出ない．
  - **macOS の CI が3回落ちて，原因は cairo だった**．
    `capabilities("cairo")` は **TRUE を返すのに `cairo_pdf()` が実行時に
    "failed to load cairo DLL" で落ち**，基本の pdf デバイスに落ちていた．
    そこで text grob ごとに「フォント名が PostScript のデータベースに無い」，
    箇条書きの記号で `mbcsToSbcs` の警告が出て，`R CMD check` が失敗していた
    (この action の既定は **警告も失敗扱い**)．
    直した内容は `tests/testthat/setup.R`:
    **実際に開いてデバイス名を見て cairo の可否を決め**，駄目なら **ragg** で測る
    (ragg は PNG 出力で使っており，システムフォントも Unicode も扱える)．
    `render_poster()` の PDF 出力そのものは cairo に依るので，**そのテストは skip** する．
  - **CI は3ジョブとも成功** (check の Ubuntu・macOS，見本のビルド)．
  - ライセンスは元から MIT．README (英・日) の末尾に節を足して3系統で揃えた．

- 2026-08-31 10:05 (このセッション，MATUTOSI_DP) その1
  **見本を4本に揃えた** (ユーザ指示の1)．acposter の4本を基準に，
  `inst/extdata/poster_sample_howto2.yml` (入力と出力の早見表) と
  `poster_sample_howto3.yml` (`grid:` の非対称な配置) を新設し，
  4本まとめて組む `inst/extdata/render_samples.R` と `inst/extdata/README.md` を添えた．
  - **`grid:` の `y` が R の yaml (YAML 1.1) で真偽値になっていた**ことが分かった
    (キーが `"TRUE"` になり `Every grid$boxes entry needs name, x, and y.` で止まる)．
    `x` は無事なので気づきにくく，**姉妹ツールと「同じ書式」のはずの `grid:` が
    ここでだけ通らなかった**．`read_poster_yaml()` が読んだ直後にキー名を戻す
    (`restore_y_key()`)．引用符付きの `'y'` でも通る．回帰テストを追加．
  - 表・図のカードは R オブジェクトが要るので，`render_samples.R` が見本4本ぶんの
    オブジェクト (図・表・配置の模式図) を作ってから渡す．
  - **検証**: 4本とも PDF に組め，新しい2本は PNG に起こして目視確認．テスト全通過．
  - **README.Rmd と vignette は触っていない** (再 knit すると 1.2MB の html まで
    作り直しになるため)．見本の一覧は `inst/extdata/README.md` に置いた．

- 2026-08-31 08:30 (x280-home)
  **統一作業の取りこぼしを塞いだ** (ユーザ指示の 2・3・7)．
  - **README (英・日) に「姉妹ツールと共通のヘッダー」節を新設**．別名・平らなヘッダー・
    `columns` は vignette と roxygen にしか書いていなかった．再 knit しても図は変わらず，
    追加分だけの差分 (英 40行・日 39行) になった．
  - **`inst/extdata/poster_sample_flat.yml` を同梱**．平らな書き方の実例が無く，
    入れ子の `poster_sample.yml` しか無かった．vignette から参照し，回帰テストも追加．
    既存の `poster_sample.yml` は**書き換えていない** (README との一致を purl で
    検証している経緯があるため，別ファイルとして足した)．
  - **`.Rbuildignore` に README の生成物を入れ，check の NOTE を消した**．
    `README.html`・`READMEjp.Rmd`・`READMEjp.html`・`READMEjp.md` が top level の
    非標準ファイルとして指摘されていた．**`R CMD check` は 0/0/0 になった**．
  - テスト151件通過．**`^vignettes$` が `.Rbuildignore` にあるため check は vignette を
    ビルドしない**ので，vignette は別途 `rmarkdown::render()` で knit を確認した．

- 2026-08-31 07:40 (x280-home)
  **acposter・qtposter と「同じヘッダーをそのまま渡せる」ようにした** (ユーザの指摘)．
  別名を受けるだけでは足りなかった．**最大の非対称は ggposter だけキーが入れ子**
  (`title:`/`poster:`/`theme:` ブロックの中) で，他2つの平らなヘッダーが通らなかった．
  - `promote_flat_keys()` を足し，**top-level に書いたキーを対応するブロックへ畳む**．
    `title:` は文字列なら表題，リストなら従来の title ブロックとして見分ける．
  - **`author`/`institute` をリストで書ける**ようにした (`collapse_title_row()` で
    `", "` 連結)．`poster_title()` は1行1文字列の前提で，リストを渡すと
    `gridtext::textbox_grob()` が落ちていた (実際に落ちて気づいた)．
  - **top-level の `columns` (`cols`) を `layout` の代わりに使える**ようにした．
    acposter・qtposter と同じ「左列を上から埋めて次の列へ」の流し込みで，
    節を書いた順に均等割りする (余りは左の列が取る)．`layout`/`grid` と併記したら警告して無視．
    `read_poster_yaml()` の検査も `columns` を認めるよう緩めた．
  - `poster-authors`・`font` の別名も追加．`normalize_aliases()` に自己別名の番人を入れた
    (`cjk_family = "cjk_family"` のような自己写像は値を消してしまう)．
  - **検証**: acposter の `golf_course.md` のヘッダーをそのまま YAML にして
    (`type:` も残したまま) 読み込み → A1・base_size 20・2列に正しく畳まれ，描画も通った．
    テスト22件追加 (全144件通過)，`R CMD check` 0 errors/0 warnings．

- 2026-08-31 05:47 (x280-home)
  **ggposter・acposter・qtposter の3系統を比較し，ヘッダーのキー名を別名で受けるようにした**．
  比較資料は `todo/.claude/notes/poster_tools.md` (横断の知見なので todo 側に置いた)．
  根の違い (構造化データ vs 散文) は統一できないが，`grid:` は ggposter と acposter で
  すでに完全に同一なので，**`grid:` を「移し替えるときの共通形」と位置づけた**
  (`layout:` は同名で別構造のまま触らない．ユーザ確定の c 案)．
  `R/aliases.R` を新設し，`author`→`authors`・`institute`/`institutes`/`affiliation`→
  `affiliations`・`note`/`footer`→`funding`・`paper`→`size`・`font-size`→`base_size` 等を
  `build_poster()` の入口で書き換える．**`size` は用紙 (ggposter) と文字サイズ (qtposter) の
  両方の意味で使われているため，どちらの別名にもしない**のが要点．
  キーと別名を両方書いたら ggposter 側を残して警告．テスト12件追加 (全122件通過)，
  `R CMD check` は 0 errors/0 warnings (NOTE 1件は `README.html` 等の既存の指摘で無関係)．
  vignette に「Moving a poster between the sibling tools」節を追加．

- 2026-08-30 13:41 (x280-home)
  **`grid:` (行またぎ `h>1` の箱) が長文だとほぼ描画されない実バグを発見・修正した**．
  vignette の実行例に見せかけの空白が多いと指摘され，最初はタイトルの高さ計算を
  疑ったが誤診断(縮小プレビューは `render_poster()` 内で `rescale_poster()` が
  丸ごと再計測するため，等倍時の値と単純比較したのが誤り．該当の題名修正は取消)．
  本当の原因は `anchor_top_left()` が `measure_width()` で幅を測ること．
  `poster_card` の幅は null 単位(列いっぱいに広がる前提)のため，親レイアウトの外で
  単独測定すると実測 16mm 相当の意味のない値になり，箱がほぼ幅ゼロに潰れて
  文字がほとんど消えていた(短文では偶然目立たなかっただけ)．
  既知の正しい幅 (`col_width_mm * b$w`) を直接使う専用配置に修正し，回帰テストを追加
  (`test-grid.R` に「keeps its full column width」)．全110件通過，`R CMD check` 0/0/0．
  vignette の例も文章量を増やし，高解像度で目視確認済み。

- 2026-08-30 07:43 (x280-home)
  **acposter の `grid: {x,y,w,h}` 相当の非対称レイアウトを実装した(`spec$grid`)**．
  `layout:` (等幅N列の単純な縦積み) とは別の入口として追加し，後方互換は維持
  (両方指定時は `grid` を優先しつつ `cli_warn`)．行の高さは非spanのカードの
  `height`/`"auto"` から決め，spanするカードは自身のサイズで左上固定(隣を伸ばさない)．
  overlap/overflow/missing はエラー，ページより高い内容は警告(acposterのページ数
  チェック相当，ユーザ確定の 1b)．`tests/testthat/test-grid.R` に15件追加(全109件通過)，
  `R CMD check` は 0 errors/warnings/notes．vignette に「Irregular layouts with
  `grid:`」節と実行例を追加し，knit して確認済み。

- 2026-08-30 07:14 (x280-home)
  **patchwork/cowplot を figure に渡せることを確認し，README ではなく vignette
  (`vignettes/ggposter.Rmd` の Content types 節) と `card_figure()` の roxygen に
  1文ずつ反映した**．cowplot もインストールして同様に動作確認済み(警告なし)．
  README.Rmd には元々「型ごとの説明」節が無いため反映していない．
  `devtools::document()` 実行時に roxygen2 のバージョン差 (7.3.3→8.1.0) による
  無関係な差分 (DESCRIPTION の RoxygenNote 等) が混ざったため，それらは元に戻し，
  `man/card_figure.Rd` の意図した差分だけ残した．テスト94件は全通過．

- 2026-08-30 06:58 (x280-home)
  **cowplot/patchwork でポスターを作れるか検討した (内部エンジンとしては非採用、入力側で検証)**．
  patchwork オブジェクト (`patchwork::wrap_plots()`) は `ggplot` を継承しているため，
  `card_figure()`・`poster()`・`render_poster()` にそのまま渡せることを確認済み (ユーザ確定，実装変更なし)．
  cowplot は次に確認する．内部を gtable から置き換える案は，
  表・画像・自由配置のテキストを扱えないため見送り．

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

### 次にやること候補

**テストの穴** (2026-09-02 の点検で洗い出した．バグは直したが，ここは手つかず)．

- **`layout$align_rows` のテストが1件も無い**．二段組み立て (測る → 建て直す) という
  最も込み入った経路で，`poster_sample_howto.yml`・`howto2`・README が使っている．
  2026-09-02 に実機で正しく動くことは確かめた (左右とも1行目が 85.4mm に揃う) が，
  **回帰を守るものが無い**．優先度は最も高い．
- **`poster_title()` のテストが1件も無い** (`logo`・`funding` の余白行・折り返し)．
- `with_caption_below()` (図の `caption:`) のテストが無い．
- `poster_register_font()` が未テスト．

**リファクタリングの候補** (同じ点検より．どれも動作は変えない)．

- `build_column()` と `build_grid_body()` で，**行の高さを決める約20行がほぼ完全な重複**
  (auto/weight → leftover → `row_mm` → 余りのスペーサ行)．
  `resolve_track_mm()` に括り出せば，「片方だけ直す」事故を防げる．
- `gpar(fill=NA, col="#FF00FF", lty="dashed", lwd=1.2)` が `R/card.R` ×2・
  `R/content.R` ×1 にコピーされている．`plot_area_grob()` 1つにまとめる．
- `build_grid_body()` (150行) が検査・測定・組み立てを1関数でやっている．
  検査部分を `validate_grid_boxes()` として切り出す．
