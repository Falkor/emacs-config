---------------

**`/!\ IMPORTANT`: This repository holds OBSOLETE configuration which are kept for archiving purposes!**

**If you are interested in my [LATEST](https://github.com/Falkor/spacemacs-config) up-to-date Emacs configuration, kindly refer to the following repository:**

<p align="center">
<b><a href="https://github.com/Falkor/spacemacs-config"><code>Falkor/spacemacs-config</code></a></b>
</p>

---------------

      Time-stamp: <Fri 2011-01-21 10:12 svarrette>

      Copyright (c) 2010-2011 Sebastien Varrette <Sebastien.Varrette@uni.lu>
                 http://varrette.gforge.uni.lu

---------------

I am an Emacs man. Some may say Emacs is a kind of religion, for me I started to
be addicted in 2000 when I arrived at the ENSIMAG and a friend of mine initiated
me. 

That's very often how it works with Emacs: someone introduces it to you, give you
its personal configuration file (i.e. its .emacs) and then you start enjoy it,
eventually add your own customization and spread again the word (and your
configuration file ;) )

In all cases, Emacs remains for me the most productive environment for
developing software in almost any language and for producing LaTeX documents.    

Recently, I decided to fully restructure my config to make it more rigourous and 
understandable. Here is the result. The latest version is available at the
following address: 
          http://github.com/Falkor/emacs-config

---------------
# Prerequisite 

You'll need to install `emacs` and a few packages. 

* Debian/Ubuntu: `apt-get install emacs23 latex-beamer ecb auctex emacs-goodies-el ecb`
* Mac OS X: There are many ways to install Emacs on Mac OS X (see [here](http://www.emacswiki.org/emacs/EmacsForMacOS) for more details). 
Until recently, I relied on [CarbonEmacs](http://homepage.mac.com/zenitani/emacs-e.html) together with the [Enhanced Carbon EMacs (ECE)](http://www.inf.unibz.it/~franconi/mac-emacs/) plugin yet I recently switched to [Aquamacs](http://aquamacs.org/) for which the current configuration should work (at least with Aquamacs version 2.1). 

*Note:* You may also rely on the [Macports](http://www.macports.org/) and install it with 

      sudo port install emacs-app cedet ecb auctex+emacs_app color-theme-mode.el

yet I do not guarantee the current configuration will work. 


---------
# Install

## Grab and setup my configuration

Get the latest version of my Emacs configuration from [github](http://github.com/Falkor/emacs-config).

You can: 

* either click on the 'Download Source' button to get a tar archive named
  `Falkor-emacs-config-xxxxxxx.tar.gz` then run:
 
           $> cd
           $> tar xvzf /path/to/Falkor-emacs-config-xxxxxxx.tar.gz
           $> mv emacs-config  ~/.emacs.d

* retrieve the latest version using git (apt-get install git-core) and running: 

           $> git clone git://github.com/Falkor/emacs-config.git
           $> mv emacs-config ~/.emacs.d

Once this is done, create the symlink to the .emacs file: 

           $> cd
           $> ln -s .emacs.d/.emacs .emacs

Now a few additional packages should be installed in ~/.emacs.d/site-lisp/
(one day, I'll make an autotools distribution to check the next items;) )

## [ELPA](http://tromey.com/elpa/) (Emacs Lisp Package Archive)

Whenever I can, I use [ELPA](http://tromey.com/elpa/) to install packages such that I don't have to bother about new versions or installation process (as I had to do for CEDET and ECB). 

My Emacs configuration is setup to use [ELPA](http://tromey.com/elpa/) such that at the first launch, some required packages will be automatically downloaded and installed (see `~/.emacs.d/init-elpa.el` for customizing the list of installed packages). 

Unfortunately, despite the growing number of packages supported by ELPA (see [ELPA News](http://tromey.com/elpa/news.html)), my configuration makes use of other packages that still need to be installed before you can use Emacs. They are detailed below. 

## [CEDET](http://cedet.sourceforge.net/) (Collection of Emacs Development Environment Tools)

[CEDET](http://cedet.sourceforge.net/) is a collection of emacs tools to make your life easier. For instance,  Semantic is in-language completion, for example.
A full install of CEDET is required for ECB (see below) and Aquamacs doesn’t come with that (yet?!) so you need to install it yourself.

The packaged version being quite old, you'll have to install and compile CEDET from scratch. 
Proceed as follows (you may also take a look at [these instructions](http://docwhat.org/2010/08/cedet-ecb-for-aquamacs/))

* ensure you removed the existing packages:

        (Debian)   $> apt-get remove --purge cedet-common cedet-contrib ecb
		(Mac OS X) $> sudo port remove ecb cedet 

* Download the latest version of [CEDET from sourceforge](http://sourceforge.net/projects/cedet/files/). Let's assume you get `cedet-1.0.tar.gz` (the one I checked):
* uncompress in the correct directory: 

      	$> cd ~/.emacs.d/site-lisp
        $> tar xvzf /path/to/cedet-1.0.tar.gz
        $> ln -s cedet-1.0 cedet

* compile CEDET (adapt the path to `Aquamacs` to reflect your own configuration):

		$> cd cedet
    	$> make EMACS=/Applications/MyTools/Aquamacs.app/Contents/MacOS/Aquamacs

* (thanks [Doctor What](http://docwhat.org/2010/08/cedet-ecb-for-aquamacs/)) Now you need to pull all the .info files into a directory called info with a dir file. Aquamacs is smart enough to figure out this is plugin specific info files, and will use it:

 		$> mkdir info
		$> cd info
		$> find .. -type f -name '*.info' | while read i; do j="$(basename $i)"; ln -s "$i" "$j"; install-info --info-dir="$(pwd)" "$j"; done
      

Now Aquamacs should have the full CEDET available, including docs.

## [ECB](http://ecb.sourceforge.net/) (Emacs Code Browser)

[ECB](http://ecb.sourceforge.net/) basically turns Emacs into a full IDE like XCode.
Again, the default packages are old so you'll need to grab the latest version.

* Download the latest version of [ECB from sourceforge](http://sourceforge.net/projects/ecb/files/). Let's assume you get `ecb-2.40.tar.gz` (the one I checked).
* uncompress ECB in the correct directory: 

		$> cd ~/.emacs.d/site-lisp
		$> tar xvzf /path/to/ecb-2.40.tar.gz 
		$> ln -s ecb-2.40 ecb
	
* (thanks [Doctor What](http://docwhat.org/2010/08/cedet-ecb-for-aquamacs/)) Symlink the info-help directory to info and create a dir file.

		$> cd ecb
		$> ln -s info-help info
		$> cd info
		$> install-info --info-dir="$(pwd)" ecb.info	

Now Aquamacs should have ECB available, including docs.
	
## [nXhtml](http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html)

[nXhtml](http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html) is an addon to Emacs for editing XHTML, PHP and similar thing. 
It is still not included in [ELPA](http://tromey.com/elpa/news.html) so we have to install it manually, yet you should be addicted to the procedure now: 

* Download the latest version of [nXhtml here](http://ourcomments.org/cgi-bin/emacsw32-dl-latest.pl) (see the link 'Download latest nXhtml (zip file)' at the bottom of the page)
* Assuming you retrieved `nxhtml-2.08-100425.zip`, run the following commands (I rename the uncompressed folder to keep track of the installed version): 

		$> cd ~/.emacs.d/site-lisp
		$> unzip /path/to/nxhtml-2.05-091202.zip
        $> mv nxhtml nxhtml-2.08
		$> ln -s nxhtml-2.08 nxhtml


## [EasyPG](see http://www.easypg.org/)

EasyPG is included in Emacs 23 so the following applies only for older versions (assuming you downloaded `epg-x.y.z.tar.gz` and `epg-x.y.z.tar.gz.sig`):

        $> gpg --verify epg-x.y.z.tar.gz.sig epg-x.y.z.tar.gz
        $> tar xvzf epg-x.y.z.tar.gz
        $> cd  epg-x.y.z
        $> ./configure --prefix=$HOME/.emacs.d/site-lisp/epg-x.y.z
        $> make install
        $> cd ~/.emacs.d/site-lisp
        $> ln -s epg-0.0.16/share/emacs/site-lisp/epg epg

## [Doxymacs](http://doxymacs.sourceforge.net/)  

This is a very nice tools for using [Doxygen](http://www.doxygen.org/) in you code. 
Assuming you download the file `doxymacs-1.8.0.tar.gz`, the installation procedure is as follows: 

		$> tar xvzf doxymacs-1.8.0.tar.gz
	    $> cd doxymacs-1.8.0
	    $> ./configure --prefix=$HOME/.emacs.d/site-lisp/doxymacs-1.8.0
	    $> make 
	    $> make install
	    $> cd ~/.emacs.d/site-lisp
	    $> ln -s doxymacs-1.8.0 doxymacs

## That's all folks! 

Now you can run emacs and ensure everything is normal. If not, as suggested, run
it again with the `--debug-init` option and check eventually the FAQ. 

Once this is done, you can byte-compile everything in the .emacs.d directory to
accelerate the startup by running 

           C-u 0 M-x byte-recompile-directory


Note: you can find a nice (and complete) tutorial on emacs basic usage  [here](http://www2.lib.uchicago.edu/keith/tcl-course/emacs-tutorial.html) 

----------------------------
# Configuration organization


		`~/.emacs.d/init.el`          A symlink to .emacs i.e. the main configuration
                              		  files 
		`~/.emacs.d/init-defuns.el`   Elisp functions definitions 
		`~/.emacs.d/init-display.el`  Configure the display (color theme etc.)
		`~/.emacs.d/init-emodes.el`   Configure the emacs modes
		`~/.emacs.d/init-elpa.el`     Configure the packages you want to install via the
                              		  Emacs Lisp Package Archive (ELPA)
		`~/.emacs.d/init-cedet.el`    Configure the CEDET environment
		`~/.emacs.d/elpa/`            Directory containing the packages installed via ELPA  
		`~/.emacs.d/insert/`          Directory that contains the templates for
                                      auto-insertion
		`~/.emacs.d/site-lisp/`       Directory containing the external elisp files used 
		`~/.emacs.d/themes/`          Directory containing the additional color theme  
		`~/.emacs.d/yasnippet/`       Directory containing my personnal snippets (see
                              		  http://emacswiki.org/Yasnippet)   

For more information, read the configurations files as I try to put relevant
information in comments.

----------------------------
# Customization

Once emacs launch correctly on your system, you'll probably want to customize the
configurations as follows:

* Edit `~/.emacs.d/init.el` and adapt the variables: 

		user-full-name
		user-mail-address
		auto-insert-organisation

---------------
# Bugs/Comments

Send me by mail (Sebastien.Varrette@uni.lu) any remark or comments. 

If you find a bug, open a new issue on [Github](https://github.com/Falkor/emacs-config/issues)



-----------
# Resources

If you want more, take a look at the following web site:

* [Emacs reference page](http://www.gnu.org/software/emacs/)
* [Emacs wiki](http://www.emacswiki.org/emacs/)
* http://emacsblog.org/
* http://www.io.com/~jimm/emacs_tips.html
* [The Emacs Starter Kit by Phil Hagelberg](http://github.com/technomancy/emacs-starter-kit/tree/master)
* [Carbon Emacs package (Mac OS X port)](http://homepage.mac.com/zenitani/emacs-e.html)
* Various `.emacs` example on the web, including

           http://www.mygooglest.com/fni/dot-emacs.html
           http://www.dotemacs.de/dotfiles/JohnJGlynn.emacs.html
           http://www-verimag.imag.fr/~moy/emacs/.emacs

* [Emacs Lisp List (ELL)](http://www.emacswiki.org/emacs/EmacsLispList) - authorative source of all Emacs packages

