{include resources/header.md}
{set-property title "Metatilities"}

<div class="contents">
<div class="system-links">

  * [Mailing Lists][3]
  * [Getting it][4]
{remark  * [Documentation][5]}
  * [News][6]
{remark  * [Test results][tr]}
  * [Changelog][7]

   [3]: #mailing-lists
   [4]: #downloads
   [5]: documentation/ (documentation link)
   [6]: #news
   [7]: changelog.html
   
</div>
<div class="system-description">

### What it is

Utilities, utilities, utilities!

Everyone needs utilities and until things like
[cl-utilities][] get going (and probably even after!),
projects need to have them nearby. Metatilities is
[metabang.com][9]'s set of core utilities. It includes much
of what you might expect and other stuff too. These are used
in [CL-Containers][], [CL-Graph][], [CL-MathStats][],
[CL-Variates][], and [TINAA][]. You are welcome to use
them too. Some documentation is [available][15] and more is
on the way.

   [cl-utilities]: http://common-lisp.net/project/cl-utilities/
   [9]: http://www.metabang.com/
   [15]: http://common-lisp.net/project/cl-containers/metatilities/documentation/

{anchor mailing-lists}

### Mailing Lists

* [devel-list][]: A list for questions, patches, bug
  reports, and so on; It's for everything other than
  announcements.

{anchor downloads}

### Where is it

metabang.com is switching from [darcs][] to [git][]
for source control; the current metatilities repository is on
[github][github-metatilities] and you can clone it using:

    git clone git://github.com/gwkkwg/metatilities

You can use [ASDF-Install][asdf-install] or just download a
[tarball][]. 
You can also follow development (such as it is :-)) on [unCLog][].

You'll need
[metatilities-base][] and [moptilities][] too. The commands
to get them are listed below:

    git clone git://github.com/gwkkwg/metatilities-base
    git clone git://github.com/gwkkwg/moptilities

metatilities is [ASDF installable][asdf-install]. Its CLiki home is
right [where][cliki-home] you'd expect.

  [18]: http://common-lisp.net/project/cl-containers/moptilities/

There's also a handy [gzipped tar file][tarball].

{anchor news}

### What is happening

2010-Dec-20 - moved to git; cleanup.
  
2009 April 29 - lots of tweaks over the past year but nothing
too exciting!

2008 May 26 - tweaks galore; cleanup and separation of
dynamic-classes and metatilities-base into their own
projects.

2007 Nov 11 - Two years, lots of changes and no news to speak
of! What is the world a'coming to.

2005 Nov 13 - Initial setup

</div>
</div>

{include resources/footer.md}

