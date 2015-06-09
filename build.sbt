organization := "edu.washington.cs.knowitall"

name := "kbp2014-slotfilling-multir"

version := "0.1"

fork := true

javaOptions in run += "-Xmx14G"

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := {_ => false}

pomExtra := (
  <url>https://github.com/knowitall/KBP2014SlotFillingMultir</url>
  <licenses>
    <license>
      <name>GNU General Public License Version 2</name>
      <url>http://www.gnu.org/licenses/gpl-2.0.txt</url>
    </license>
  </licenses>
 <scm>
   <url>https://github.com/knowitall/KBP2014SlotFillingMultir</url>
   <connection>scm:git://github.com/knowitall/KBP2014SlotFillingMultir.git</connection>
   <developerConnection>scm:git:git@github.com:knowitall/KBP2014SlotFillingMultir.git</developerConnection>
   <tag>HEAD</tag>
 </scm>
    <developers>
       <developer>
          <name>John Gilmer</name>
       </developer>
    </developers>)


publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if(v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2") }

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

val stanfordNlp = "edu.stanford.nlp" % "stanford-corenlp" % "3.4" artifacts (Artifact("stanford-corenlp", "models"), Artifact("stanford-corenlp"))

libraryDependencies ++= Seq(
  stanfordNlp,
  "com.typesafe" % "config" % "1.2.1",
  "org.apache.commons" % "commons-io" % "1.3.2",
  "commons-lang" % "commons-lang" % "2.6",
  "commons-cli" % "commons-cli" % "1.2",
//  "edu.stanford.nlp" % "stanford-corenlp" % "1.3.4",
  "com.guokr" % "stan-cn-nlp" % "0.0.4",
  "org.codehaus.jackson" % "jackson-mapper-asl" % "1.8.5",
  "org.apache.derby" % "derby" % "10.10.1.1",
  "org.apache.derby" % "derbyclient" % "10.9.1.0",
  "jp.sf.amateras.solr.scala" %% "solr-scala-client" % "0.0.7",
  "edu.washington.cs.knowitall" % "multir-framework_2.10" % "0.3-SNAPSHOT" withSources() withJavadoc(),
  "edu.washington.cs.knowitall.common-scala" % "common-scala_2.10" % "1.1.2",
  "edu.washington.cs.knowitall.taggers" %% "taggers" % "0.1" excludeAll(ExclusionRule(organization = "com.googlecode.clearnlp")),
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-sentence-breeze" % "2.4.2" excludeAll(ExclusionRule(organization = "com.googlecode.clearnlp")),
  "com.googlecode.clearnlp" % "clearnlp-threadsafe" % "1.3.0-c",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-chunk-opennlp" % "2.4.2",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-parse-clear" % "2.4.2" excludeAll(ExclusionRule(organization = "com.googlecode.clearnlp"))
//  "edu.washington.cs.knowitall.stanford-corenlp" % "stanford-ner-models" % "1.3.4",
//  "edu.washington.cs.knowitall.stanford-corenlp" % "stanford-postag-models" % "1.3.4",
//  "edu.washington.cs.knowitall.stanford-corenlp" % "stanford-dcoref-models" % "1.3.4",
//  "edu.washington.cs.knowitall.stanford-corenlp" % "stanford-parse-models" % "1.3.4",
//  "edu.washington.cs.knowitall.stanford-corenlp" % "stanford-sutime-models" % "1.3.4",
//  "edu.stanford.nlp.chinese" % "stanford-chinese-corenlp" % "1.0"
)

resolvers ++= Seq(
  "amateras-repo" at "http://amateras.sourceforge.jp/mvn/",
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")
