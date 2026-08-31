# 見本 (samples)

ポスターを作るツールは3系統ある (ggposter・[acposter](https://github.com/matutosi/acposter)・
[qtposter](https://github.com/matutosi/qtposter))．
**3つとも同じ内容・同じ順の見本を4本持っている** (2026-08-31 に揃えた)．
同じポスターを3つの書き方で書くとどうなるかを見比べられる．

| | ggposter | acposter | qtposter |
|---|---|---|---|
| 1. カードの種類の一巡り | `poster_sample_howto.yml` | `examples/poster_howto.md` | `poster_howto.qmd` |
| 2. 入力と出力の早見表 | `poster_sample_howto2.yml` | `examples/poster_howto2.md` | `poster_howto2.qmd` |
| 3. 非対称な配置 (`grid:`) | `poster_sample_howto3.yml` | `examples/poster_howto3.md` | `poster_howto3.qmd` |
| 4. 実際のポスターに近い例 | `poster_sample.yml` | `examples/golf_course.md` | `golf_course.qmd` |

このほかに `poster_sample_flat.yml` (平らなヘッダーの書き方) と
`poster_readme_example.yml` (README で使う最小の例) がある．

## 組み方

表や図のカードは R オブジェクトを要るので (YAML は「何をどこに置くか」だけを持ち，
解析は R に残す)，同梱の `render_samples.R` がそれらを作ってから組む．

```r
source(system.file("extdata", "render_samples.R", package = "ggposter"))
render_ggposter_samples(out_dir = ".")             # 4本とも
render_ggposter_samples(".", which = "howto3")     # 1本だけ
```

見本 3 (`poster_sample_howto3.yml`) は本文だけなので，オブジェクトなしで組める．

```r
render_poster(
  poster(system.file("extdata", "poster_sample_howto3.yml", package = "ggposter")),
  "poster_sample_howto3.pdf"
)
```

## `grid:` を姉妹ツールから移すとき

`grid:` の書式は3つとも同じ (`columns` ＋ `boxes` の `name`/`x`/`y`/`w`/`h`，0 起点)．
**R の yaml は YAML 1.1 なので，引用符の無い `y` を真偽値として読む**が，
`read_poster_yaml()` が読み込んだ直後に戻すので，
`- {name: a, x: 0, y: 0}` のまま移してよい (`'y'` と書いても通る)．

## 縮小画像

README (英・日) に載せる縮小画像は，組んだ PDF から作る．

```
pdftoppm -r 26 -png -f 1 -l 1 -singlefile poster_sample_howto.pdf man/figures/README-sample1
```

`-r 26` は A1 のポスターが 600 px 幅ほどになる解像度で，A0 を `-r 18` で起こす
acposter・qtposter の縮小画像とほぼ同じ大きさになる．
置き場所が `previews/` ではなく `man/figures/` なのは，**pkgdown が参照サイトへ
複写するのがこのディレクトリだけ**のため (ここだけ姉妹ツールと違う)．

## README が使う例

`poster_readme_example.yml` と対になる R のリストは `readme_example.R` にある．
README にコードを直接置かず，ファイルへ切り出してリンクする形にした (2026-08-31)．
