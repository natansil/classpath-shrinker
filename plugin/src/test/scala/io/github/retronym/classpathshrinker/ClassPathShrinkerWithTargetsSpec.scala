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
  def `error on indirect dependency target`(): Unit = {
    val testCode =
      """object Foo {
        |  org.apache.commons.lang3.ArrayUtils.EMPTY_BOOLEAN_ARRAY.length
        |}
      """.stripMargin
    val commonsPath = Coursier.getArtifact(commons)
    val commonsTarget = "//commons:Target".encode()
    val indirect = Map(commonsPath -> commonsTarget)
    run(testCode, withIndirect = indirect).expectErrorOn(indirect(commonsPath).decoded)
  }

  @Test
  def `error on multiple indirect dependency targets`(): Unit = {
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
    run(testCode, withIndirect = indirect).expectErrorOn(commonsTarget, guavaTarget)
  }

  @Test
  def `do not give error on direct dependency target`(): Unit = {
    val testCode =
      """object Foo {
        |  org.apache.commons.lang3.ArrayUtils.EMPTY_BOOLEAN_ARRAY.length
        |}
      """.stripMargin
    val commonsPath = Coursier.getArtifact(commons)
    val commonsTarget = "commonsTarget"

    val direct = Seq(commonsPath)
    val indirect = Map(commonsPath -> commonsTarget)
    run(testCode, withDirect = direct, withIndirect = indirect).noErrorOn(commonsTarget)
  }


  implicit class `nice errors on sequence of strings`(infos: Seq[String]) {

    private def checkErrorContainsMessage(target: String) = (_: String).contains(targetErrorMessage(target))

    private def targetErrorMessage(target: String) = s"Target '$target' is used but isn't explicitly declared, please add it to the deps"

    def expectErrorOn(targets: String*) = targets.foreach(target => assert(
      infos.exists(checkErrorContainsMessage(target)),
      s"expected an error on $target to appear in errors!")
    )

    def noErrorOn(target: String) = assert(
      !infos.exists(checkErrorContainsMessage(target)),
      s"error on $target should not appear in errors!")

    def expectNoJarMessageOn(unusedJar: String) = assert(
      !infos.exists(_.contains(ClassPathFeedback.createWarningMsg(Seq(unusedJar)))),
      "should not give error on unused jars when using targets!")
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
