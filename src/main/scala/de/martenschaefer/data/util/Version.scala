package de.martenschaefer.data.util

import de.martenschaefer.data.serialization.Codec
import de.martenschaefer.data.util.Either._

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

    given Codec[Undefined] = Codec[String].xmap[Undefined](Undefined(_))(_.version)

    given Codec[Version] = Codec[Either[Either[Simple, Semver], Undefined]].xmap(_ match {
        case Left(either2) => either2 match {
            case Left(Simple(version)) => Simple(version)
            case Right(Semver(major, minor, patch, preRelease)) => Semver(major, minor, patch, preRelease)
        }
        case Right(Undefined(version)) => Undefined(version)
    })(_ match {
        case Simple(version) => Left(Left(Simple(version)))
        case Semver(major, minor, patch, preRelease) => Left(Right(Semver(major, minor, patch, preRelease)))
        case Undefined(version) => Right(Undefined(version))
    })
}
