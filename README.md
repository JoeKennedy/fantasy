## Haskell Setup

### Install Stack

I use Stack to manage this repo due to its integration with the
[Heroku Buildpack for Stack](https://github.com/mfine/heroku-buildpack-stack).
Stack setup steps for Yesod can be found on the
[Yesod book's quick start guide](https://www.yesodweb.com/page/quickstart);
more general installation steps can be found on
[Stack's website](https://docs.haskellstack.org/en/stable/README/). Mac users
can use Homebrew:

```bash
brew install stack
```

## Fantasy App Setup

### Clone repo

First `cd` to the directory that houses your Git and/or Yesod projects, and run

```bash
git clone https://github.com/JoeKennedy/fantasy.git
```

### Install yesod-bin

In order to run `yesod` commands in your terminal, you'll need to install
yesod-bin

```bash
cabal install yesod-bin
```

### Initial compile

With Stack, just `cd` to your local copy of the repo and run:

```bash
stack build
```

### Create database

Run `psql` to create the database. Then, in the postgres shell, run:

```bash
CREATE USER fantasy password 'fantasy';
CREATE DATABASE fantasy OWNER fantasy;
\q
```

### Set up .env variables

The current (as of 05/18/2017) environment variables necessary to run the app
are:

```bash
AWS_ACCESS_KEY
AWS_SECRET_KEY
GOOGLE_OAUTH2_CLIENT_ID
GOOGLE_OAUTH2_CLIENT_SECRET
FACEBOOK_OAUTH2_APP_ID
FACEBOOK_OAUTH2_APP_SECRET
LETS_ENCRYPT_ACME_CHALLENGE
LETS_ENCRYPT_SECRET
```

Send me an email at joseph.stephen.kennedy@gmail.com to get the values for those
variables. Or just set them to whatever you'd like and use Yahoo to sign in for
local testing.

At some point I'll have to make a dev account for AWS, Google OAuth2, and
Facebook OAuth2.

### Start app

Pretty simple, just run:

```bash
yesod devel
```

This will create all the tables, indices, and unique constraints, and start the
development server. To quit the server process, type `quit`.

## Contribute

Take a look at one of the [issues](https://github.com/JoeKennedy/fantasy/issues)
filed on Github. If it's tagged with "help wanted", feel free to take a swipe at
it! If you can think of another improvement and want to submit that, feel free
to do that as well! Or, if you've found a bug, feel free tofile it. You know the
drill.
