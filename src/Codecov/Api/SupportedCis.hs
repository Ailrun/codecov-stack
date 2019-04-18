module Codecov.Api.SupportedCis where

data Type
  = Travis
  | Buildbot
  | Circleci
  | Buddybuild
  | Solano
  | Teamcity
  | Appveyor
  | Wercker
  | Magnum
  | Shippable
  | Codeship
  | Droneio
  | Jenkins
  | Semaphore
  | Gitlab
  | Bamboo
  | Snap
  | Buildkite
  | Bitrise
  | Greenhouse
  | Custom
  deriving ( Eq
           )

instance Show Type where
  show Travis = "travis"
  show Buildbot = "buildbot"
  show Circleci = "circleci"
  show Buddybuild = "buddybuild"
  show Solano = "solano"
  show Teamcity = "teamcity"
  show Appveyor = "appveyor"
  show Wercker = "wercker"
  show Magnum = "magnum"
  show Shippable = "shippable"
  show Codeship = "codeship"
  show Droneio = "drone.io"
  show Jenkins = "jenkins"
  show Semaphore = "semaphore"
  show Gitlab = "gitlab"
  show Bamboo = "bamboo"
  show Snap = "snap"
  show Buildkite = "buildkite"
  show Bitrise = "bitrise"
  show Greenhouse = "greenhouse"
  show Custom = "custom"
