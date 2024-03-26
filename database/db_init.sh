#!/bin/bash
set -e

# Running SQL scripts to create app db
echo ""
echo "PUBLIC SCHEMA SETUP..."
psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" --file /tmp/sql/public_setup.sql
echo "DONE"

echo ""
echo "PUBLIC TABLE CREATION..."
psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" --file /tmp/sql/public_tables.sql
echo "DONE"

echo ""
echo "LOADING TEST DATA..."
psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" --file /tmp/sql/public_load_data.sql
echo "DONE"

# echo ""
# echo "PUBLIC USER CREATION..."
# psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" --file /tmp/sql/public_users.sql
# echo "DONE"
echo ""