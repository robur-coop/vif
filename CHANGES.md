### v0.0.1~beta2 (2026-02-23)

- Add `Vif.Queries.all` (@reynir, [#5][5])
- Improve examples and `README.md` (@dinosaure, @reynir, [#7][7], [#9][9])
- Rename `Vif.Uri.path` to `Vif.Uri.path` (@dinosaure, @reynir, [#8][8])
- Fix errors when we parse `multipart/form-data` (@dinosaure, spotted by @yomimono, [#14][14], [#22][22])
- Catch exceptions from URI converters (@reynir, @dinosaure, [#12][12])
- Add `?etag` argument for files (@reynir, [#16][16])
- Be able to have a regular expression per HTTP methods (@reynir, [#18][18])
- Add `Vif.Uri.execp` and `Vif.Uri.extract` (@reynir, [#20][20])
- Exclude `?` from ``Vif.Uri.string `Path`` (@reynir, [#23][23])
- Add `Vif.Response.with_text` (@dinosaure, [#24][24], [#37][37])
- Fix the support of MacOS (@vvvvv, @dinosaure, spotted by @voodoos, [#25][25])
- Introduce `vifu`, a unikernel compatible version of `vif` (@dinosaure, [#28][28])
- Fix `Vif.Response.with_file` (@dinosaure, [#29][29])
- Fix a double-close on `Vif.Response.with_json` and how the function is exposed (@dinosaure, [#30][30], [#37][37])
- Let the user to define a log reporter (@voodoos, @dinosaure, [#32][32])
- Fix the support of h2 protocol and which field we should generate (@dinosaure, [#33][33])
- Use [`flux`](https://github.com/robur-coop/flux) (@dinosaure, [#34][34])
- Fix memory leak with `logs` (@dinosaure, [#38][38], [#39][39])
- Update to `tyre.1.0.0` (@dinosaure, @EmileTrotignon, [#40][40])
- Remove the usage of ppx in our tests (@dinosaure, [#42][42])
- Verify the TLS configuration given (and its ALPN protocols) (@dinosaure, spotted by @voodoos, [#41][41])
- Provide `Vif.Response.with_stream` (@dinosaure, [#43][43])
- Remove deprecated `jsonm` dependency (@dinosaure, [#45][45])
- Give the possibility for the user to initialize RNG (@dinosaure, requested by @voodoos, [#46][46])
- Remove pin-depends (@hannesm, @dinosaure, [#48][48])

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
