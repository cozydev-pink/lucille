import xerial.sbt.Sonatype.sonatypeCentralHost

// https://typelevel.org/sbt-typelevel/faq.html#what-is-a-base-version-anyway
ThisBuild / tlBaseVersion := "0.0" // your current series x.y

ThisBuild / organization := "pink.cozydev"
ThisBuild / organizationName := "CozyDev"
ThisBuild / startYear := Some(2022)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List(
  // your GitHub handle and name
  tlGitHubDev("valencik", "Andrew Valencik"),
  tlGitHubDev("samspills", "Sam Pillsworth"),
)

// use Sonatype Central
ThisBuild / sonatypeCredentialHost := sonatypeCentralHost

// publish website from this branch
ThisBuild / tlSitePublishBranch := Some("main")

// use JDK 11
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("11"))
ThisBuild / tlJdkRelease := Some(8)

val Scala212 = "2.12.20"

val Scala213 = "2.13.16"

ThisBuild / crossScalaVersions := Seq(Scala212, Scala213, "3.3.6")

ThisBuild / scalaVersion := Scala213 // the default Scala

lazy val root = tlCrossRootProject.aggregate(core, benchmarks)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "lucille",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-parse" % "1.1.0",
      "org.typelevel" %%% "cats-core" % "2.13.0",
      "org.scalameta" %%% "munit" % "1.1.1" % Test,
    ),
  )

lazy val benchmarks = project
  .in(file("benchmarks"))
  .dependsOn(core.jvm)
  .settings(
    name := "lucille-benchmarks"
  )
  .enablePlugins(NoPublishPlugin, JmhPlugin)

import laika.ast.Path.Root
import laika.helium.config.{IconLink, HeliumIcon, TextLink, ThemeNavigationSection}
lazy val docs = project
  .in(file("site"))
  .enablePlugins(TypelevelSitePlugin)
  .dependsOn(core.jvm)
  .settings(
    tlSiteApiPackage := Some("lucille"),
    tlSiteHelium :=
      tlSiteHelium.value.site.darkMode.disabled.site
        .topNavigationBar(
          homeLink = IconLink.external("https://github.com/cozydev-pink/lucille", HeliumIcon.home)
        )
        .site
        .mainNavigation(
          appendLinks = Seq(
            ThemeNavigationSection(
              "Related Projects",
              TextLink.external("https://lucene.apache.org/", "lucene"),
              TextLink.external("https://typelevel.org/cats-parse/", "cats-parse"),
              TextLink.external("https://typelevel.org/cats/", "cats"),
            )
          )
        ),
  )
