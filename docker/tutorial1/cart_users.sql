create table cart_users (
  email      varchar not null,
  first_name varchar not null,
  last_name  varchar not null,
  password   varchar not null,
  primary key (email)
);
