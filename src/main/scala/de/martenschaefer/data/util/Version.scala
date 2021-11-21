package de.martenschaefer.data.util

import java.util.Objects
import de.martenschaefer.data.serialization.{ Codec, ValidationError }
import de.martenschaefer.data.util.DataResult.*

enum Version extends Ordered[Version] {
    case Simple(val version: Int)
    case Semver(val major: Int, val minor: Int, val patch: Int, preRelease: List[String] = List.empty)

    override def compare(that: Version): Int = this match {
        case Simple(version) => that match {
            case Simple(version2) => version - version2
            case Semver(major, minor, patch, preRelease) =>
                if (version == major && minor == 0 && patch == 0 && !preRelease.isEmpty) 1
                else version - major
        }
        case Semver(major, minor, patch, preRelease: List[String]) => that match {
            case Simple(version) =>
                if (version == major && minor == 0 && patch == 0 && preRelease.nonEmpty) -1
                else major - version
            case Semver(major2, minor2, patch2, preRelease2: List[String]) =>
                if (major == major2)
                    if (minor == minor2)
                        if (patch == patch2)
                            preRelease.zip(preRelease2).find((pr1, pr2) => pr1.compareTo(pr2) != 0)
                                .map((pr1, pr2) => pr1.compareTo(pr2)).getOrElse(preRelease2.size - preRelease.size)
                        else
                            patch - patch2
                    else
                        minor - minor2
                else
                    major - major2
        }
    }

    override def toString: String = this match {
        case Simple(version) => version.toString
        case Semver(major, minor, patch, preRelease) =>
            val withoutPre = s"$major.$minor.$patch"

            if (preRelease.isEmpty)
                withoutPre
            else
                s"$withoutPre${preRelease.mkString("-", ".", "")}"
    }

    override def equals(o: Any): Boolean = this match {
        case Simple(version) => o match {
            case Simple(version2) => version == version2
            case Semver(major, 0, 0, Nil) => version == major
            case _ => false
        }

        case Semver(major, minor, patch, preRelease) => o match {
            case Simple(version) => major == version && minor == 0 && patch == 0 && preRelease.isEmpty

            case Semver(major2, minor2, patch2, preRelease2) => major == major2 && minor == minor2 && patch == patch2
                && preRelease == preRelease2

            case _ => false
        }
    }
}

object Version {
    given Codec[Simple] = Codec[Int].xmap[Simple](Simple.apply)(_.version)

    private val simpleCodec: Codec[Version] = Codec[Simple].flatXmap(Success(_)) {
        case simple: Simple => Success(simple)
        case _ => Failure(List(ValidationError(path => s"$path: Version is not a simple number", List.empty)))
    }

    given Codec[Semver] = Codec.record {
        val major = Codec[Int].fieldOf("major").forGetter[Semver](_.major)
        val minor = Codec[Int].fieldOf("minor").forGetter[Semver](_.minor)
        val patch = Codec[Int].fieldOf("patch").forGetter[Semver](_.patch)
        val preRelease = Codec[Option[List[String]]].xmap {
            case Some(list) => list
            case None => List()
        }(list => if (list.isEmpty) None else Some(list)).fieldOf("pre_release").forGetter[Semver](_.preRelease)

        Codec.build(Semver(major.get, minor.get, patch.get, preRelease.get))
    }

    private val semverCodec: Codec[Version] = Codec[Semver].flatXmap(Success(_)) {
        case semver: Semver => Success(semver)
        case _ => Failure(List(ValidationError(path => s"$path: Not a semver version", List.empty)))
    }

    given Codec[Version] = Codec.alternatives(List(simpleCodec, semverCodec))
}
