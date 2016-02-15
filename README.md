_A minimal yesod REST server boilerplate._

## Defaults

* Configured to use *Postgres*.
* Supports *Bearer token* authentication out-of-the-box.
* Includes *User* and *API Key* models.
* Does **not** enable *CSRF protection* by default, since no session or cookie
  based authentication is provided.
* Static assets are mounted on the root path `/`.
* Uses [**stack**][stack] by default.

## Features

* *Explicit dependency-based database migrations* backed by [moo][moo].
* *Migration hints* when there is a schema mismatch, rather than running
  migrations automatically.
* Simple mechanism for validating and updating entities safely.
* Assortments of helpers for handlers and tests that I've found useful.

## How to

### Setup

Install [stack][stack] first, and then run:

```
stack build --exec server
```

### Run tests

```
stack test
```

### Create migrations

Running `stack test` will error out with something like this if you get a schema
mismatch:

```sh
Migrations required. Consider using these:
CREATE TABLE "comment"("id" SERIAL  PRIMARY KEY UNIQUE,"message" VARCHAR NOT NULL,"user_id" INT8 NOT NULL);
ALTER TABLE "comment" ADD CONSTRAINT "comment_user_id_fkey" FOREIGN KEY("user_id") REFERENCES "user"("id");
```

So create a new migration in the `migrations` folder with the above content. You
can use the `moo` tool to scaffold for you:

```
stack install dbmigrations # If you haven't already
moo new add-comment
```

You should end up with something like the following:

```yaml
Depends: user
Apply: |
  CREATE TABLE "comment"("id" SERIAL  PRIMARY KEY UNIQUE,"message" VARCHAR NOT NULL,"user_id" INT8 NOT NULL);
  ALTER TABLE "comment" ADD CONSTRAINT "comment_user_id_fkey" FOREIGN KEY("user_id") REFERENCES "user"("id");
```

The migration above will run on the next test run or server launch.

[stack]: https://github.com/commercialhaskell/stack
[moo]: https://github.com/jtdaugherty/dbmigrations
