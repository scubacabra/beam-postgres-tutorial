create table cart_users (
  email      varchar not null primary key,
  first_name varchar not null,
  last_name  varchar not null,
  password   varchar not null
);

create table cart_user_addresses (
  id serial       primary key,
  address1        varchar not null,
  address2        varchar,
  city            varchar not null,
  state           varchar not null,
  zip             varchar not null,
  for_user__email varchar not null references cart_users (email)
);
