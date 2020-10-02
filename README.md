## Overview
This repo serves as documentation of code used by the routine reporting team to generate IM slide 1 during the COVID-19 response 
for the Case Surveillance Section. Intially, this slide (IM slide 1) was generated in excel but 
was transitioned to R during July 2020. Within this repo, each of these files discussed
below is also included as example input and output. 

Once all packages are installed, one can just 'source' the entire [script](https://github.com/CDCgov/IMslide/tree/master/code) to generate
3 different outputs after adjusting the date for the file names of the 3 outputs (L164, 
L294, and L381). 
Those outputs include: 

* 2 [png images](https://github.com/CDCgov/IMslide/tree/master/images) 
* 1 [excel file](https://github.com/CDCgov/IMslide/tree/master/updated.data) with updated information generated in R

The png images are [epi-curves](https://www.cdc.gov/foodsafety/outbreaks/investigating-outbreaks/epi-curves.html); one with correction to exclude high number of probable 
deaths reported during June (slide2). The input to generate these files is a csv file
consisting of 5 columns - submission date, total cases, new cases, new deaths, and total deaths.
This file was generated daily from DCIPHER and made available to those on the routine reporting team. 

This code was developed while on COVID19 response by 

* [Jenna Hamlin](https://github.com/jennahamlin)
* [Marcela Torres](https://www.linkedin.com/in/marcela-torres24)

As of this writing and because we are no longer on the response changes may have been
made to generate these images, but hope that this repo could be helpful for others. We 
encourage others that are on response and participating in making these figures to 
provide addtional updates as they see fit. 


--------------------------------------------------------------------------------------------
  
#### Public Domain Standard Notice
This repository constitutes a work of the United States Government and is not
subject to domestic copyright protection under 17 USC § 105. This repository is in
the public domain within the United States, and copyright and related rights in
the work worldwide are waived through the [CC0 1.0 Universal public domain dedication](https://creativecommons.org/publicdomain/zero/1.0/).
All contributions to this repository will be released under the CC0 dedication. By
submitting a pull request you are agreeing to comply with this waiver of
copyright interest.

#### License Standard Notice
The repository utilizes code licensed under the terms of the Apache Software
License and therefore is licensed under ASL v2 or later.

This source code in this repository is free: you can redistribute it and/or modify it under
the terms of the Apache Software License version 2, or (at your option) any
later version.

This source code in this repository is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the Apache Software License for more details.

You should have received a copy of the Apache Software License along with this
program. If not, see http://www.apache.org/licenses/LICENSE-2.0.html

The source code forked from other open source projects will inherit its license.

#### Privacy Standard Notice
This repository contains only non-sensitive, publicly available data and
information. All material and community participation is covered by the
[Disclaimer](https://github.com/CDCgov/template/blob/master/DISCLAIMER.md)
and [Code of Conduct](https://github.com/CDCgov/template/blob/master/code-of-conduct.md).
For more information about CDC's privacy policy, please visit [http://www.cdc.gov/other/privacy.html](https://www.cdc.gov/other/privacy.html).

#### Contributing Standard Notice
Anyone is encouraged to contribute to the repository by [forking](https://help.github.com/articles/fork-a-repo)
and submitting a pull request. (If you are new to GitHub, you might start with a
[basic tutorial](https://help.github.com/articles/set-up-git).) By contributing
to this project, you grant a world-wide, royalty-free, perpetual, irrevocable,
non-exclusive, transferable license to all users under the terms of the
[Apache Software License v2](http://www.apache.org/licenses/LICENSE-2.0.html) or
later.

All comments, messages, pull requests, and other submissions received through
CDC including this GitHub page may be subject to applicable federal law, including but not limited to the Federal Records Act, and may be archived. Learn more at [http://www.cdc.gov/other/privacy.html](http://www.cdc.gov/other/privacy.html).

#### Records Management Standard Notice
This repository is not a source of government records, but is a copy to increase
collaboration and collaborative potential. All government records will be
published through the [CDC web site](http://www.cdc.gov).

#### Additional Standard Notices
Please refer to [CDC's Template Repository](https://github.com/CDCgov/template)
for more information about [contributing to this repository](https://github.com/CDCgov/template/blob/master/CONTRIBUTING.md),
[public domain notices and disclaimers](https://github.com/CDCgov/template/blob/master/DISCLAIMER.md),
and [code of conduct](https://github.com/CDCgov/template/blob/master/code-of-conduct.md).
