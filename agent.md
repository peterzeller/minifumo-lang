# Codex development setup

Use the following commands to install sbt locally (per the Codex environment setup) and run tests:

```bash
curl -L -o sbt-1.10.10.zip \
  https://github.com/sbt/sbt/releases/download/v1.10.10/sbt-1.10.10.zip
unzip sbt-1.10.10.zip
export PATH="$PWD/sbt/bin:$PATH"

sbt test
```
