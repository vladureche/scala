#!/usr/bin/env bash
#
##############################################################################
# Copyright 2002-2011, LAMP/EPFL
#
# This is free software; see the distribution for copying conditions.
# There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.
##############################################################################

default_java_opts="-Xmx768m -Xms768m"
bootcp=true
programName=$(basename "$0")

declare -a java_args scala_args residual_args
unset verbose quiet cygwin toolcp colors saved_stty

ReplMain=scala.tools.nsc.MainGenericRunner
CompilerMain=scala.tools.nsc.Main
ScaladocMain=scala.tools.nsc.ScalaDoc
FscMain=scala.tools.nsc.CompileClient
ScalapMain=scala.tools.scalap.Main

ifdebug () {
  [[ -n $debug ]] && eval "$@"
}
echoErr () {
  echo >&2 "$@"
}
dlog () {
  [[ -n $debug ]] && echoErr "$@"
}

die() {
  echo "Aborting: $@"
  exit 1
}
echoArgs () {
  echoErr ""
  for arg; do
    echoErr "$arg"
  done
  echoErr ""
}
execCommand () {
  ifdebug echoArgs "$@"
  "$@"
}

# restore stty settings (echo in particular)
restoreSttySettings () {
  dlog "" && dlog "[restore stty] $saved_stty"
  stty $saved_stty && saved_stty=""
}

onExit () {
  [[ -n $saved_stty ]] && restoreSttySettings
  exit $scala_exit_status
}

# Get debug set early
for arg in "$@"; do
  [[ $arg == "-d" ]] && debug=true
done

# to reenable echo if we are interrupted before completing.
trap onExit INT

# save terminal settings
saved_stty="$(stty -g 2>/dev/null)"

# clear on error so we don't later try to restore them
[[ $? ]] || saved_stty=""
dlog "[save stty] $saved_stty"

if uname | grep -q ^CYGWIN; then
  cygwin="$(uname)"
fi

addJava () {
  dlog "[addJava] arg = '$1'"
  java_args=( "${java_args[@]}" "$1" )
}
addScala () {
  dlog "[addScala] arg = '$1'"
  scala_args=( "${scala_args[@]}" "$1" )
}
addResidual () {
  dlog "[residual] arg = '$1'"
  residual_args=( "${residual_args[@]}" "$1" )
}

findScalaHome () {
  local source="$1"
  local libjar="$(dirname "$source")/../lib/scala-library.jar"

  if [[ -f "$libjar" ]]; then
    ( cd -P "$(dirname "$source")/.." && pwd )
  elif [[ -h "$source" ]]; then
    local linked="$(readlink "$source")"
    local dir="$( cd -P $(dirname "$source") && cd -P $(dirname "$linked") && pwd )"
    findScalaHome "$dir/$(basename "$linked")"
  fi
}

# mkString : foo bar baz  ==> foo:bar:baz
mkString () {
  sep="$1" && shift && ( IFS="$sep" ; echo "$*"; )
}

onExit() {
  [[ -n $saved_stty ]] && restoreSttySettings
  exit $scala_exit_status
}

# to reenable echo if we are interrupted before completing.
trap onExit INT

# Finding the root folder for this Scala distribution
scala_home="$(findScalaHome "${BASH_SOURCE[0]}")"

# Constructing the java classpath
toolClasspath () {
  [[ -n "$toolcp" ]] && echo "$toolcp" || mkString : $scala_home/lib/*
}

# If using the boot classpath, also pass an empty classpath
# to java to suppress "." from materializing.
classpathArgs () {
  if [[ -n $bootcp ]]; then
    echo "-Xbootclasspath/a:$(toolClasspath) -classpath \"\""
  else
    echo "-classpath $(toolClasspath)"
  fi
}

# e.g. path -java-home /path/to/java_home
require_arg () {
  local type="$1"
  local opt="$2"
  local arg="$3"

  if [[ -z "$arg" ]] || [[ "${arg:0:1}" == "-" ]]; then
    die "$opt requires <$type> argument"
  fi
}

case $programName in
    scala) scala_main_class=$ReplMain ;;
   scalac) scala_main_class=$CompilerMain ;;
      fsc) scala_main_class=$FscMain ;;
 scaladoc) scala_main_class=$ScaladocMain ;;
   scalap) scala_main_class=$ScalapMain ;;
        *) unset scala_main_class ;;
esac

while [[ $# -gt 0 ]]; do
  case "$1" in
           --) shift; for arg; do addResidual "$arg"; done; set -- ;;
     -h|-help) usage; exit 1 ;;
  -v|-verbose) verbose=true && shift ;;
    -d|-debug) debug=true && shift ;;
    -q|-quiet) quiet=true && shift ;;

        -repl) scala_main_class=$ReplMain && shift ;;
     -compile) scala_main_class=$CompilerMain && shift ;;
         -run) scala_main_class=$ReplMain && shift ;;
         -fsc) scala_main_class=$FscMain && shift ;;
      -bootcp) bootcp=true && shift ;;
   -no-bootcp) unset bootcp && shift ;;
      -colors) colors=true && shift ;;
   -no-colors) unset colors && shift ;;
      -jrebel) jrebel=true && shift ;;
   -no-jrebel) unset jrebel && shift ;;

      -toolcp) require_arg classpath "$1" "$2" && toolcp="$2" && shift 2 ;;
   -java-home) require_arg path "$1" "$2" && java_cmd="$2/bin/java" && shift 2 ;;

          # break out -D and -J options and add them to JAVA_OPTS as well
          # so they reach the JVM in time to do some good.  The -D options
          # will be available as system properties.
          -D*) addJava "$1" && addScala "$1" && shift ;;
          -J*) addJava "${1:2}" && addScala "$1" && shift ;;
            *) addResidual "$1" && shift ;;
  esac
done

[[ -z $java_cmd ]] && prefix=${java_home:+$java_home/bin/} && java_cmd="${prefix}java"

# note that variables which may intentionally be empty must not
# be quoted: otherwise an empty string will appear as a command line
# argument, and java will think that is the program to run.
execCommand \
  "$java_cmd" \
  ${JAVA_OPTS:-$default_java_opts} \
  "${java_args[@]}" \
  $(classpathArgs) \
  -Dscala.home="$scala_home" \
  -Dscala.usejavacp=true \
  "${scala_main_class:-$ReplMain}" \
  "${scala_args[@]}" \
  "${residual_args[@]}"

# record the exit status lest it be overwritten:
# then reenable echo and propagate the code.
scala_exit_status=$?
onExit
