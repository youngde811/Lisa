# -*- mode: shell-script; sh-shell: bash -*-

# MIT License

# Copyright (c) 2000 David Young

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

readonly progname="$(basename $0)"

set -e -o pipefail

workdir=

usage() {
    cat <<EOF
Usage: $progname [OPTIONS]

This script can be used to install and configure Quicklisp for
your SBCL environment. All Quicklisp installation defaults are taken,
so if you want a different installation experience, please do all of
this manually.

Options:
    -h    Show this message and exit.
EOF

    exit 1
}

setup_quicklisp() {
    local ql="$workdir/quicklisp.lisp"
    local ql_asc="$workdir/quicklisp.lisp.asc"
    
    curl -o $ql https://beta.quicklisp.org/quicklisp.lisp
    curl -o $ql_asc https://beta.quicklisp.org/quicklisp.lisp.asc

    gpg --verify $ql_asc $ql || echo "$progname: could not verify $ql"

    sbcl --eval "(load \"$ql\")" --eval "(quicklisp-quickstart:install)" --eval "(ql:system-apropos :log4cl)" --eval "(ql:add-to-init-file)" --eval "(sb-ext:exit)"
}

sanity_checks() {
    which -s curl || { echo "$progname: curl is required; please install it."; exit 1; }
    which -s gpg || { echo "$progname: gpg is required; please install it."; exit 1; }
    which -s sbcl || { echo "$progname: sbcl is required; please install it."; exit 1; }
}

# -- main() --

while getopts "h" opt ; do
    case $opt in
        h) usage ;;
        *) usage ;;
    esac
done

workdir="$(mktemp -d /tmp/ql.XXXXX)"

trap "rm -rf $workdir" EXIT

sanity_checks

setup_quicklisp

exit 0
