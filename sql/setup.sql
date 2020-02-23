create table if not exists Artist (
  id text primary key,
  href text not null,
  follower_href text,
  follower_count integer not null,
  artist_name text not null,
  popularity integer not null,
  uri text not null,
  created_at timestamp default current_timestamp
);

create table if not exists Genre (
  genre_name text not null,
  created_at timestamp default current_timestamp,
  primary key (genre_name)
    on conflict ignore
);

create table if not exists ArtistGenre (
  artist_id text not null,
  genre_name text not null,
  created_at timestamp default current_timestamp,
  primary key (artist_id, genre_name),
  foreign key (artist_id) references Artist (id)
    on delete cascade,
  foreign key (genre_name) references Genre (genre_name)
    on delete cascade
);

create table if not exists ArtistImage (
  id integer primary key,
  href text not null,
  height integer not null,
  width integer not null,
  artist_id text not null,
  created_at timestamp default current_timestamp,
  foreign key (artist_id) references Artist (id)
    on delete cascade
);

create table if not exists ArtistRelation (
  artist1_id text not null,
  artist2_id text not null,
  created_at timestamp default current_timestamp,
  primary key (artist1_id, artist2_id),
  foreign key (artist1_id) references Artist (id)
    on delete cascade,
  foreign key (artist2_id) references Artist (id)
    on delete cascade
);
