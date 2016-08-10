# shell_expansion

[![Build Status](https://travis-ci.org/sinkuu/shell_expansion.svg?branch=master)](https://travis-ci.org/sinkuu/shell_expansion)

Shell-ish parameter expansion

## Supported expansion format

Expansion pattern | Description
--- | ---
`${parameter}`, `$parameter` | Substitute `parameter`.
`${parameter:-word}`, `${parameter-word}` | If `parameter` is unset, the expansion of `word` is substituted. If the colon is used, null values are treated as unset. The same shall apply to below.
`${parameter:=word}`, `${parameter=word}` | If `parameter` is unset, the expansion of`word`is assigned to parameter. In all cases the value of`parameter`is substituted.
`${parameter:?[word]}`, `${parameter?[word]}` | If `paramter` is unset, `Expander::expand` returns error `ExpanderError::ParameterNotSet`.
`${parameter+word}` | If paramter is unset, nothing is substituted; otherwise, the expansion of`word`is substituted.
`${#parameter}`, `$#parameter` | The length of`parameter`is substituted.
`${parameter%pattern}`, `${parameter%%pattern}`, `${parameter#pattern}`,  `${parameter##pattern}` | The`parameter`is substituted with its prefix(`#`, `##`) or suffix(`%`, `%%`) matched by glob pattern deleted. `#`/`%` for shortest matching, and `##`/`%%` for  longest matching.
