  $ $JBUILDER runtest -j0 --root . 2>&1 | sed "s/ cmd /  sh /"
            sh stderr,stdout
            sh stderr,stdout
          diff alias runtest
          diff alias runtest
            sh both
            sh both
          diff alias runtest
