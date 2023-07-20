DRONES_DIR = $(shell git config "borg.drones-directory" || echo "modules")

-include $(DRONES_DIR)/borg/borg.mk

help helpall::
	$(info )
	$(info Bootstrapping)
	$(info -------------)
	$(info make bootstrap-borg  = make borg and make targets available)
	@printf "\n"

bootstrap-borg:
	@mkdir .git/modules
	@git clone git@github.com:emacscollective/borg.git $(DRONES_DIR)/borg \
	--separate-git-dir .git/modules/borg
	@cd $(DRONES_DIR)/borg; git symbolic-ref HEAD refs/heads/main
	@cd $(DRONES_DIR)/borg; git reset --hard HEAD
