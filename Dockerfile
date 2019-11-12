FROM swipl

ARG USER_NAME=boaz
ARG USER_HOME=/home/boaz
ARG USER_ID=639163
ARG USER_GECOS=LaTeX

RUN adduser \
  --home "$USER_HOME" \
  --uid $USER_ID \
  --gecos "$USER_GECOS" \
  --disabled-password \
  "$USER_NAME"

WORKDIR /home/boaz

