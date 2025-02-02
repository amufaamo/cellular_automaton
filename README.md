# cellular_automaton
RのShinyでセルラーオートマトンを作ってみた

## セルラーオートマトンとは？
セルラーオートマトン（Cellular Automaton、CA）は、離散的なモデルの一種で、個々のセルが規則に従って時間とともに変化するシステムです。CAは次のような特徴を持ちます：

グリッド構造: セルが配置されるグリッド（通常は2次元）があり、それぞれのセルは特定の状態（例えば、0か1の2つの状態）を持ちます。

局所的なルール: 各セルの次の状態は、そのセル自身と近傍のセルの現在の状態に基づいて決定されます。これらのルールは一様であり、全てのセルに同じものが適用されます。

離散的な時間ステップ: システムは一連の離散的な時間ステップに基づいて進行し、各ステップごとに全てのセルが同時に更新されます。

## 例: ライフゲーム（Conway's Game of Life）
ライフゲームは、イギリスの数学者ジョン・コンウェイが考案した有名なセルラーオートマトンです。次のようなルールに従います：

生存: 生きているセル（1）の周りに2または3つの生きている隣接セルがある場合、そのセルは次のステップでも生き残ります。
過疎: 生きているセルの周りに生きている隣接セルが1つ以下の場合、そのセルは次のステップで死にます（0になります）。
過密: 生きているセルの周りに生きている隣接セルが4つ以上の場合、そのセルは次のステップで死にます。
再生: 死んでいるセル（0）の周りにちょうど3つの生きている隣接セルがある場合、そのセルは次のステップで生き返ります（1になります）。
この簡単なルールセットから、複雑で予測不可能なパターンが生まれ、セルラーオートマトンの研究は複雑系や自己組織化の研究に重要な洞察を提供します。
![スクリーンショット 2024-07-09 15 10 24](https://github.com/amufaamo/cellular_automaton/assets/45814539/63db4177-841b-4bd6-ac1c-194e951f4ef7)
