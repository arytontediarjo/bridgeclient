% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/participants.R
\name{get_participant}
\alias{get_participant}
\title{Get a study participant (user) record using the external ID of the account}
\usage{
get_participant(
  user_id = NULL,
  external_id = NULL,
  health_code = NULL,
  study_id = NULL,
  phone = NULL,
  email = NULL
)
}
\arguments{
\item{user_id}{The user ID of the account to delete.}

\item{external_id}{The external ID of the participant}

\item{health_code}{The healthcode of the account to delete.}

\item{study_id}{The study ID. Must be set with user_id
to have an effect. Will use the v5 APIs (rather than v3).}

\item{phone}{The participant's phone number.}

\item{email}{The participant's email address.}
}
\description{
All of the parameters are optional, but at least one participant
identifier must be supplied. If multiple identifiers are supplied,
only the first identifier (as determined by the function definition)
will be used. If a phone number or email address is supplied, there
can only be one matching user, else an error is thrown.
}
