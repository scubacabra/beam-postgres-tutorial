create table cart_users (
  email      varchar not null primary key,
  first_name varchar not null,
  last_name  varchar not null,
  password   varchar not null
);

create table addresses (
  id              serial primary key,
  address1        varchar not null,
  address2        varchar,
  city            varchar not null,
  state           varchar not null,
  zip             varchar not null,
  for_user__email varchar not null references cart_users (email)
);

create table products (
  id          serial primary key,
  title       varchar not null,
  description varchar not null,
  price       integer not null
);

create table orders (
  id                  serial primary key,
  date                timestamp without time zone not null,
  for_user__email     varchar not null references cart_users (email),
  ship_to_address__id integer not null references addresses (id),
  shipping_info__id   integer /* this is a nullable foreign key as per the tutorial */
);

create table shipping_info (
  id              serial primary key,
  carrier         varchar not null,
  tracking_number varchar not nulL
);

create table line_items (
  item_in_order__id    integer not null references orders (id),
  item_for_product__id integer not null references products (id),
  item_quantity        integer not nulL
);
