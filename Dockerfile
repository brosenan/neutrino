FROM swipl

RUN apt-get update
RUN apt-get install -y python3 clang git procps

ARG USER_NAME=user
ARG USER_HOME=/home/user
# Put your local user-ID here.
ARG USER_ID=639163
ARG USER_GECOS=LaTeX

RUN adduser \
  --home "$USER_HOME" \
  --uid $USER_ID \
  --gecos "$USER_GECOS" \
  --disabled-password \
  "$USER_NAME"

WORKDIR /home/user
