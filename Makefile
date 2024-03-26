WATERSHED_SERVICE=watershedbc
POSTGIS_SERVICE=postgis
PGADMIN_SERVICE=pgadmin


default: help

help:  ## Display this help
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m<target>\033[0m\n\nTargets:\n"} /^[a-zA-Z0-9_-]+:.*?##/ { printf "  \033[36m%-10s\033[0m %s\n", $$1, $$2 }' $(MAKEFILE_LIST)

fresh_install: ## Builds and starts all services
	@docker compose stop
	@docker compose up -d

restart_all: ## Restarts all services
	@docker compose restart

restart_app: ## Restarts Watershed app
	@docker compose restart $(WATERSHED_SERVICE)

run_db: ## Build and run only the local database
	@docker compose stop
	@docker compose up -d $(POSTGIS_SERVICE)

run_db_app: ## Builds and runs the DB and Watershed app
	@docker compose stop
	@docker compose up -d $(POSTGIS_SERVICE) $(WATERSHED_SERVICE)

run_db_admin: ## Builds and runs the DB and PGAdmin
	@docker compose stop
	@docker compose up -d $(POSTGIS_SERVICE) $(PGADMIN_SERVICE)

logs_app: ## Streams the Watershed app logs
	@docker compose logs -f $(WATERSHED_SERVICE)

logs_db: ## Streams the DB app logs
	@docker compose logs -f $(POSTGIS_SERVICE)

shell_app: ## Watershed app container shell
	@docker compose exec $(WATERSHED_SERVICE) bash

shell_db: ## DB container shell
	@docker compose exec $(POSTGIS_SERVICE) bash

kill: ## Kills and removes all services
	@docker compose kill
	@docker compose rm -f

prune: ## Docker system prune
	@docker system prune --volumes --force
