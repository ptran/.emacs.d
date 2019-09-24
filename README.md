# Emacs Config

Personal Emacs configuration with fairly straightforward structuring. `init.el` loads in settings from the `config` directory. Aside from `global_settings.el`, each settings file is modularized by either programming language development or major mode.

Despite the various packages used, the following are essential for daily software development:

- [use-package](https://github.com/jwiegley/use-package) - manage all the packages
- [ivy](https://github.com/abo-abo/swiper) - fast completion front-end
- [projectile](https://github.com/bbatsov/projectile) - amazing project management and navigation tools
- [magit](https://github.com/magit/magit) - best git interface ever
- [evil](https://github.com/emacs-evil/evil) - vim modal editing
