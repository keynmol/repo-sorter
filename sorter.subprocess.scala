import cue4s.Prompts
import cue4s.SyncPrompts
import cue4s.CompletionError
import scala.util.Try

@main def sort_repos(pathStr: String) =
  val root = os.Path(pathStr, os.pwd)

  Prompts.sync.use: prompts =>
    val candidates =
      os.walk
        .stream(root, skip = path => os.isFile(path), maxDepth = 1)
        .filter(path => os.exists(path / ".git")) // Only Git repositories
        .toList

    scribe.info(s"Found ${candidates.length} candidates")

    candidates
      .foreach: path =>
        scribe.info(s"Visiting $path")
        val remotes = getRemotes(path)
        scribe.info(s"Remotes: $remotes")

        remotes.toList match
          case (name, repo) :: Nil =>
            scribe.info(s"Only one remote, will move to ${root / repo}")
            Try(move(prompts)(path, root, repo)).failed.toOption
              .foreach(scribe.error("Failed to move", _))

          case Nil =>
            scribe.error(s"$path is not a Git repo, not doing anything")

          case options =>
            scribe.warn(
              s"$path has multiple remotes, a choice will be required"
            )
            val choice = prompts
              .singleChoice(
                "Which remote is the right one",
                options.map(_._2.toString)
              )
              .toEither

            choice match
              case Left(CompletionError.Interrupted) =>
                scribe.info(s"Skipping $path")
              case Left(other) => throw other
              case Right(value) =>
                val repo = os.RelPath.fromStringSegments(value.split("/"))
                Try(move(prompts)(path, root, repo)).failed.toOption
                  .foreach(scribe.error("Failed to move", _))
end sort_repos

def move(prompts: SyncPrompts)(path: os.Path, root: os.Path, repo: os.RelPath) =
  val destination = root / repo
  val destinationOrg = root / repo / os.up
  if os.exists(destination) then
    scribe.error(
      s"Destination [$destination] already exists, doing nothing"
    )
  else
    os.makeDir.all(destinationOrg)
    val shouldMove =
      prompts.confirm(s"Will move $path to $destination").getOrThrow

    if shouldMove then
      scribe.info(s"Moving $path to $destination")
      os.move(path, destination)
      val shouldDelete =
        prompts.confirm(s"Delete $path").getOrThrow

      if shouldDelete then os.remove(path)
end move

def getRemotes(path: os.Path) =
  val proc = os.proc("git", "remote", "-v").call(cwd = path).out.text()
  proc.linesIterator.toList
    .flatMap: line =>
      line.split("\t").toList match
        case name :: s"git@github.com:$org/$repo.git $rest" :: Nil =>
          Some(name -> os.RelPath.fromStringSegments(Array(org, repo)))
        case name :: s"git@github.com:$org/$repo.git $rest" :: Nil =>
          Some(name -> os.RelPath.fromStringSegments(Array(org, repo)))
        case segments =>
          scribe.warn(
            s"Can't parse git remote line `$line` (segments: $segments)"
          )
          None
    .toMap
