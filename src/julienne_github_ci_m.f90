! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
module julienne_github_ci_m
  !! Detect whether a program is running in GitHub Continuous Integration (CI)
  implicit none
   
  interface

    logical module function GitHub_CI()
      !! The result is true if the environment variable named "CI" is set to the string "true"
    end function

  end interface

end module
