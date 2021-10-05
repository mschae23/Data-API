package de.martenschaefer.data.util

import de.martenschaefer.data.serialization.{ Codec, ValidationError }
import de.martenschaefer.data.util.DataResult.*

enum Version extends PartiallyOrdered[Version] {
    case Simple(val version: Int)
    case Semver(val major: Int, val minor: Int, val patch: Int, preRelease: List[String])
    case Undefined(val version: String)

    override def tryCompareTo[B >: Version](that: B)(using AsPartiallyOrdered[B]): Option[Int] = this match {
        case Simple(version) => that match {
            case Simple(version2) => Some(version - version2)
            case Semver(major, _, _, preRelease) => Some(version - major)
                .map(x => if (version == major && !preRelease.isEmpty) 1 else x)
            case Undefined(_) => None
        }
        case Semver(major, minor, patch, preRelease) => that match {
            case Simple(version) => Some(major - version)
                .map(x => if (version == major && !preRelease.isEmpty) -1 else x)
            case Semver(major2, minor2, patch2, preRelease2) => Some(major - major2)
                .map(x => if (major == major2) minor - minor2 else x)
                .map(x => if (minor == minor2) patch - patch2 else x)
                .map(x => if (patch == patch2) preRelease.zip(preRelease2).find((pr1, pr2) => pr1.compareTo(pr2) != 0)
                    .map((pr1, pr2) => pr1.compareTo(pr2)).getOrElse(0) else x)
            case Undefined(_) => None
        }
        case Undefined(_) => None
    }
}

object Version {
    given Codec[Simple] = Codec[Int].xmap[Simple](Simple(_))(_.version)

    private val simpleCodec: Codec[Version] = Codec[Simple].flatXmap(Success(_))(_ match {
        case simple: Simple => Success(simple)
        case _ => Failure(List(ValidationError(path => s"$path: Version is not a simple number", List.empty)))
    })

    given Codec[Semver] = Codec.record {
        val major = Codec[Int].fieldOf("major").forGetter[Semver](_.major)
        val minor = Codec[Int].fieldOf("minor").forGetter[Semver](_.minor)
        val patch = Codec[Int].fieldOf("patch").forGetter[Semver](_.patch)
        val preRelease = Codec[Option[List[String]]].xmap(_ match {
            case Some(list) => list
            case None => List()
        })(list => if (list.isEmpty) None else Some(list)).fieldOf("pre_release").forGetter[Semver](_.preRelease)

        Codec.build(Semver(major.get, minor.get, patch.get, preRelease.get))
    }

    private val semverCodec: Codec[Version] = Codec[Semver].flatXmap(Success(_))(_ match {
        case semver: Semver => Success(semver)
        case _ => Failure(List(ValidationError(path => s"$path: Not a semver version", List.empty)))
    })

    given Codec[Undefined] = Codec[String].xmap[Undefined](Undefined(_))(_.version)

    private val undefinedCodec: Codec[Version] = Codec[Undefined].flatXmap(Success(_))(_ match {
        case undefined: Undefined => Success(undefined)
        case _ => Failure(List(ValidationError(path => s"$path: Version is not using an undefined version String", List.empty)))
    })

    given Codec[Version] = Codec.alternatives(List(simpleCodec, semverCodec, undefinedCodec))
}
