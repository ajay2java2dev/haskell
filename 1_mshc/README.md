# Some basic commands On Mac
- brew --version
- brew install haskell-stack
- stack --version
- which stack

# To create a new project
- stack new hello-world simple --resolver=lts-7.8
- stack build - run this command inside the hello-world folder
- stack setup - incase there is any error
- stack exec hello-world - this should return hello-world since it was part of the template we downloaded and is there withing src/Main.hs
