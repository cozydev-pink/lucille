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

// publish to s01.oss.sonatype.org (set to true to publish to oss.sonatype.org instead)
ThisBuild / tlSonatypeUseLegacyHost := false

// publish website from this branch
ThisBuild / tlSitePublishBranch := Some("main")

// publish snapshots from main branch
ThisBuild / tlCiReleaseBranches := Seq("main")

// use JDK 11
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("11"))
ThisBuild / tlJdkRelease := Some(8)

val Scala212 = "2.12.18"
val Scala213 = "2.13.10"
ThisBuild / crossScalaVersions := Seq(Scala212, Scala213, "3.3.0")
ThisBuild / scalaVersion := Scala213 // the default Scala

lazy val root = tlCrossRootProject.aggregate(core)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "lucille",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-parse" % "0.3.9",
      "org.typelevel" %%% "cats-core" % "2.9.0",
      "org.scalameta" %%% "munit" % "1.0.0-M7" % Test,
    ),
  )

import laika.ast.Path.Root
import laika.helium.config.{IconLink, HeliumIcon, TextLink, ThemeNavigationSection}
import cats.data.NonEmptyList
lazy val docs = project
  .in(file("site"))
  .enablePlugins(TypelevelSitePlugin)
  .dependsOn(core.jvm)
  .settings(
    tlSiteApiPackage := Some("lucille"),
    tlSiteHelium := {
      tlSiteHelium.value.site.darkMode.disabled.site
        .topNavigationBar(
          homeLink = IconLink.external("https://github.com/cozydev-pink/lucille", HeliumIcon.home)
        )
        .site
        .mainNavigation(
          appendLinks = Seq(
            ThemeNavigationSection(
              "Related Projects",
              NonEmptyList.of(
                TextLink.external("https://lucene.apache.org/", "lucene"),
                TextLink.external("https://typelevel.org/cats-parse/", "cats-parse"),
                TextLink.external("https://typelevel.org/cats/", "cats"),
              ),
            )
          )
        )
    },
  )
