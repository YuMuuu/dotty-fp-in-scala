# FP in Scala on Dotty
dotty で fp in scala をやるリポジトリだよ！

現状dotty独自の機能として
- Extension Methods
- Top level definitions
- Improved type inference
をつかってるよ

## sbt project compiled with Dotty

### Usage

This is a normal sbt project, you can compile code with `sbt compile` and run it
with `sbt run`, `sbt console` will start a Dotty REPL.

For more information on the sbt-dotty plugin, see the
[dotty-example-project](https://github.com/lampepfl/dotty-example-project/blob/master/README.md).
