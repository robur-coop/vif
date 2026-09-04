### v0.0.1~beta5 (2026-09-04)

- Fix `ETag` (@reynir, [!75][75])
- Fix how we manipulate the target on our handlers (@dinosaure, [!76][76])
- Upgrade to mhttp.0.0.3 (@dinosaure, [!77][77])
- Add `Vif{,u}.Route.query` (@dinosaure, [!65][65])

[65]: https://git.robur.coop/robur/vif/pulls/65
[75]: https://git.robur.coop/robur/vif/pulls/75
[76]: https://git.robur.coop/robur/vif/pulls/76
[77]: https://git.robur.coop/robur/vif/pulls/77

### v0.0.1~beta4 (2026-07-31)

- Handle `Host` fields and be able to dispatch requests with our `Vif.Uri` DSL
  (spotted by @theAlexes, fixed by @dinosaure, [!63][63], [#35][g35])
- Update to `caqti.3.0.0` (@dinosaure, [!64][64])
- Add statistics (@hannesm, @dinosaure, @reynir, [!69][69])
- Fix & improve documentation (@hannesm, @mneumann, [#37][g37], [!68][68])
- Use `mirage-crytpo-rng.unix` instead of `mirage-crypto-rng-miou-unix`
  (@dinosaure, @hannesm, [!71][71])
- Use `waitport` instead of `waitfile` (@dinosaure, [!72][72])
- Export `Vif.Request.tags` to get tags from a request (@reynir, [!66][66])

[63]: https://git.robur.coop/robur/vif/pulls/63
[64]: https://git.robur.coop/robur/vif/pulls/64
[69]: https://git.robur.coop/robur/vif/pulls/69
[68]: https://git.robur.coop/robur/vif/pulls/68
[71]: https://git.robur.coop/robur/vif/pulls/71
[72]: https://git.robur.coop/robur/vif/pulls/72
[66]: https://git.robur.coop/robur/vif/pulls/66
[g35]: https://github.com/robur-coop/vif/issues/35
[g37]: https://github.com/robur-coop/vif/pull/37

### v0.0.1~beta3 (2026-05-06)

- Delete `mirage-crypto-rng-miou-unix` dependency for `vif.core`\
  (@dinosaure, 3fb86c5)
- Improve performances, when we handle requests without body, we executed them
  directly instead of transferring the handler into a new domain
  (@dinosaure, #25, [!51][51])
- Catch exceptions raised by `jsont` (@dinosaure, [!52][52])
- Use `jws` instead of `jwto` (@dinosaure, [!53][53])
- Handle TLS flow with `vifu` (@dinosaure, [!54][54])
- Expose `Vifu.Uri.{execp,extract}` (@swrup, #26)
- Use `Flux.Source.with_buffered_formatter` (instead of `with_formatter`)
  (@reynir, [!55][55])
- Fix the compilation with `caqti.2.3.0` (@dinosaure, [!57][57])
- Get ride of `rresult` (@reynir, [!58][58])
- Depends on `flux.0.0.1~beta5` (@reynir, [!59][59])
- Be able to stop a Vif server (as we do for `httpcats`)
  (@theAlexes, @dinosaure, #29)
- Be able to specify a UNIX domain socket as our listener for our webserver
  (@theAlexes, @dinosaure, @reynir, #30, #31)
- Use `httpcats.0.3.0` and shutdown our connection at the end
  (@dinosaure, [!60][60])
- Add `OPTIONS` method for our routes (@voodoos, #33)
- Revert [!60][60] and fix unexpected closed connection (see #32) and when we
  would like to transmit big file ([!61][61], spotted by @voodoos, @dinosaure,
  @theAlexes)
- Don't emit `application/octet-stream` for unrecognized files
  (spotted by @n-osborne, @dinosaure, #27, #28)

[51]: https://git.robur.coop/robur/vif/pulls/51
[52]: https://git.robur.coop/robur/vif/pulls/52
[53]: https://git.robur.coop/robur/vif/pulls/53
[54]: https://git.robur.coop/robur/vif/pulls/54
[55]: https://git.robur.coop/robur/vif/pulls/55
[57]: https://git.robur.coop/robur/vif/pulls/57
[58]: https://git.robur.coop/robur/vif/pulls/58
[59]: https://git.robur.coop/robur/vif/pulls/59
[60]: https://git.robur.coop/robur/vif/pulls/60
[61]: https://git.robur.coop/robur/vif/pulls/61

### v0.0.1~beta2 (2026-02-23)

- Add `Vif.Queries.all` (@reynir, [!5][5])
- Improve examples and `README.md` (@dinosaure, @reynir, [!7][7], [!9][9])
- Rename `Vif.Uri.path` to `Vif.Uri.path` (@dinosaure, @reynir, [!8][8])
- Fix errors when we parse `multipart/form-data` (@dinosaure, spotted by
  @yomimono, [!14][14], [!22][22])
- Catch exceptions from URI converters (@reynir, @dinosaure, [!12][12])
- Add `?etag` argument for files (@reynir, [!16][16])
- Be able to have a regular expression per HTTP methods (@reynir, [!18][18])
- Add `Vif.Uri.execp` and `Vif.Uri.extract` (@reynir, [!20][20])
- Exclude `?` from ``Vif.Uri.string `Path`` (@reynir, [!23][23])
- Add `Vif.Response.with_text` (@dinosaure, [!24][24], [!37][37])
- Fix the support of MacOS (@vvvvv, @dinosaure, spotted by @voodoos, [!25][25])
- Introduce `vifu`, a unikernel compatible version of `vif`
  (@dinosaure, [!28][28])
- Fix `Vif.Response.with_file` (@dinosaure, [!29][29])
- Fix a double-close on `Vif.Response.with_json` and how the function is exposed
  (@dinosaure, [!30][30], [!37][37])
- Let the user to define a log reporter (@voodoos, @dinosaure, [!32][32])
- Fix the support of h2 protocol and which field we should generate
  (@dinosaure, [!33][33])
- Use [`flux`](https://github.com/robur-coop/flux) (@dinosaure, [!34][34])
- Fix memory leak with `logs` (@dinosaure, [!38][38], [!39][39])
- Update to `tyre.1.0.0` (@dinosaure, @EmileTrotignon, [!40][40])
- Remove the usage of ppx in our tests (@dinosaure, [!42][42])
- Verify the TLS configuration given (and its ALPN protocols) (@dinosaure,
  spotted by @voodoos, [!41][41])
- Provide `Vif.Response.with_stream` (@dinosaure, [!43][43])
- Remove deprecated `jsonm` dependency (@dinosaure, [!45][45])
- Give the possibility for the user to initialize RNG (@dinosaure,
  requested by @voodoos, [!46][46])
- Remove pin-depends (@hannesm, @dinosaure, [!48][48])

[5]: https://git.robur.coop/robur/vif/pulls/5
[7]: https://git.robur.coop/robur/vif/pulls/7
[9]: https://git.robur.coop/robur/vif/pulls/9
[8]: https://git.robur.coop/robur/vif/pulls/8
[14]: https://git.robur.coop/robur/vif/pulls/14
[22]: https://git.robur.coop/robur/vif/pulls/22
[12]: https://git.robur.coop/robur/vif/pulls/12
[16]: https://git.robur.coop/robur/vif/pulls/16
[18]: https://git.robur.coop/robur/vif/pulls/18
[20]: https://git.robur.coop/robur/vif/pulls/20
[23]: https://git.robur.coop/robur/vif/pulls/23
[24]: https://git.robur.coop/robur/vif/pulls/24
[37]: https://git.robur.coop/robur/vif/pulls/37
[25]: https://git.robur.coop/robur/vif/pulls/25
[28]: https://git.robur.coop/robur/vif/pulls/28
[29]: https://git.robur.coop/robur/vif/pulls/29
[30]: https://git.robur.coop/robur/vif/pulls/30
[37]: https://git.robur.coop/robur/vif/pulls/37
[32]: https://git.robur.coop/robur/vif/pulls/32
[33]: https://git.robur.coop/robur/vif/pulls/33
[34]: https://git.robur.coop/robur/vif/pulls/34
[38]: https://git.robur.coop/robur/vif/pulls/38
[39]: https://git.robur.coop/robur/vif/pulls/39
[40]: https://git.robur.coop/robur/vif/pulls/40
[42]: https://git.robur.coop/robur/vif/pulls/42
[41]: https://git.robur.coop/robur/vif/pulls/41
[43]: https://git.robur.coop/robur/vif/pulls/43
[45]: https://git.robur.coop/robur/vif/pulls/45
[46]: https://git.robur.coop/robur/vif/pulls/46
[48]: https://git.robur.coop/robur/vif/pulls/48

### v0.0.1~beta1 (2025-08-14)

- First release of Vif
