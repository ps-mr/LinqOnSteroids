import sbt._

object MyPlugins extends Build {
    lazy val root = Project("root", file(".")) dependsOn (sampleapp, junitXmlListener)
    lazy val junitXmlListener =
        uri("git://github.com/ijuma/junit_xml_listener.git#fe434773255b451a38e8d889536ebc260f4225ce")
    lazy val sampleapp = Project(id = "los-sampleapp-for-generation",
        base = file("sampleapp"))
}

// vim: set ts=4 sw=4 et:
