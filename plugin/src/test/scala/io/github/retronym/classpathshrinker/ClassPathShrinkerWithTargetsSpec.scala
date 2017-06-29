package io.github.retronym.classpathshrinker

import coursier.{Dependency, Module}
import io.github.retronym.classpathshrinker.TestUtil._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class ClassPathShrinkerWithTargetsSpec {

  object Dependencies {
    val commons =
      Dependency(Module("org.apache.commons", "commons-lang3"), "3.5")
    val guava = Dependency(Module("com.google.guava", "guava"), "21.0")
  }

  import Dependencies._

  @Test
  def `unused jars are not reported when targets are used`(): Unit = {
    val testCode =
      """object Demo"""
    val commonsPath = Coursier.getArtifact(commons)
    val commonsTarget = "commonsTarget"
    val indirect = Map(commonsPath -> commonsTarget)
    run(testCode, withIndirect = indirect).expectNoJarMessageOn(commonsPath)
  }


  @Test
  def `warn on indirect dependency target`(): Unit = {
    val testCode =
      """object Foo {
        |  org.apache.commons.lang3.ArrayUtils.EMPTY_BOOLEAN_ARRAY.length
        |}
      """.stripMargin
    val commonsPath = Coursier.getArtifact(commons)
    val commonsTarget = "//commons:Target".encode()
    val indirect = Map(commonsPath -> commonsTarget)
    run(testCode, withIndirect = indirect).expectWarningOn(indirect(commonsPath).decoded)
  }

  @Test
  def `warn on multiple indirect dependency targets`(): Unit = {
    val testCode =
      """object Foo {
        |  org.apache.commons.lang3.ArrayUtils.EMPTY_BOOLEAN_ARRAY.length
        |  com.google.common.base.Strings.commonPrefix("abc", "abcd")
        |}
      """.stripMargin
    val commonsPath = Coursier.getArtifact(commons)
    val commonsTarget = "commonsTarget"

    val guavaPath = Coursier.getArtifact(guava)
    val guavaTarget = "guavaTarget"

    val indirect = Map(commonsPath -> commonsTarget, guavaPath -> guavaTarget)
    run(testCode, withIndirect = indirect).expectWarningOn(commonsTarget, guavaTarget)
  }

  @Test
  def `do not warn on direct dependency target`(): Unit = {
    val testCode =
      """object Foo {
        |  org.apache.commons.lang3.ArrayUtils.EMPTY_BOOLEAN_ARRAY.length
        |}
      """.stripMargin
    val commonsPath = Coursier.getArtifact(commons)
    val commonsTarget = "commonsTarget"

    val direct = Seq(commonsPath)
    val indirect = Map(commonsPath -> commonsTarget)
    run(testCode, withDirect = direct, withIndirect = indirect).noWarningsOn(commonsTarget)
  }


  implicit class `nice warnings on sequence of strings`(warnings: Seq[String]) {

    private def checkWarningContainsMessage(target: String) = (_: String).contains(targetWarningMessage(target))

    private def targetWarningMessage(target: String) = s"target '$target' should be added to deps"

    def expectWarningOn(targets: String*) = targets.foreach(target => assert(
      warnings.exists(checkWarningContainsMessage(target)),
      s"expected a warning on $target to appear in warnings!")
    )

    def noWarningsOn(target: String) = assert(
      !warnings.exists(checkWarningContainsMessage(target)),
      s"warning on $target should not appear in warnings!")

    def expectNoJarMessageOn(unusedJar: String) = assert(
      !warnings.exists(_.contains(ClassPathFeedback.createWarningMsg(Seq(unusedJar)))),
      "should not warn on unused jars when using targets!")
  }

  implicit class `decode bazel lables`(targetLabel: String) {
    def decoded() = {
      targetLabel.replace(";", ":")
    }

    def encode() = {
      targetLabel.replace(":", ";")
    }
  }
}
