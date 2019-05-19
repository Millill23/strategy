CREATE TABLE IF NOT EXISTS player
(
    id BIGSERIAL,
    nickname TEXT NOT NULL,
    password TEXT NOT NULL,
    played_battles BIGINT DEFAULT 0,
    battles_won BIGINT DEFAULT 0,
    created timestamp without time zone DEFAULT timezone('UTC'::text, now()),
    updated timestamp without time zone DEFAULT timezone('UTC'::text, now()),

    CONSTRAINT player_id_pkey PRIMARY KEY (id),
    CONSTRAINT player_nickname_key UNIQUE (nickname)
);

CREATE TYPE currency_type AS ENUM('gold', 'diamonds');
CREATE TABLE IF NOT EXISTS money
(
    id BIGSERIAL,
    player_id BIGINT NOT NULL,
    currency_code currency_type,
    amount BIGINT NOT NULL,
    created timestamp without time zone DEFAULT timezone('UTC'::text, now()),
    updated timestamp without time zone DEFAULT timezone('UTC'::text, now()),

    CONSTRAINT money_id_pkey PRIMARY KEY (id),
    CONSTRAINT money_unique_key UNIQUE (player_id, currency_code)
);


CREATE TYPE achievement_type AS ENUM('first win','10 wins');
CREATE TABLE IF NOT EXISTS achievement
(
    id BIGSERIAL,
    player_id BIGINT NOT NULL,
    achievement_code achievement_type,
    state BOOLEAN,
    created timestamp without time zone DEFAULT timezone('UTC'::text, now()),
    updated timestamp without time zone DEFAULT timezone('UTC'::text, now()),

    CONSTRAINT achievement_id_pkey PRIMARY KEY (id),
    CONSTRAINT achievement_unique_key UNIQUE (player_id, achievement_code)
);



CREATE TABLE IF NOT EXISTS auth_token
(
    id BIGSERIAL,
    player_id BIGINT NOT NULL,
    token TEXT NOT NULL,
    expired_at timestamp without time zone,
    created timestamp without time zone DEFAULT timezone('UTC'::text, now()),
    updated timestamp without time zone DEFAULT timezone('UTC'::text, now()),

    CONSTRAINT auth_token_id_pkey PRIMARY KEY (id),
    CONSTRAINT auth_token_unique_key UNIQUE (player_id, token)
);


CREATE TYPE log_action_type AS ENUM('loging', 'registration', 'battle', 'achievement', 'battle_win', 'money_earn','transaction');
CREATE TABLE IF NOT EXISTS log_action
(
    id BIGSERIAL,
    player_id BIGINT NOT NULL,
    action_type log_action_type,
    action_data JSONB,
    created timestamp without time zone DEFAULT timezone('UTC'::text, now()),

    CONSTRAINT log_action_id_pkey PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS battle
(
    id BIGSERIAL,
    currency_code currency_type DEFAULT 'gold',
    bid BIGINT DEFAULT 0,
    player_id_1 BIGINT,
    player_id_2 BIGINT,
    winner_id BIGINT,
    created timestamp without time zone DEFAULT timezone('UTC'::text, now()),

    CONSTRAINT battle_id_pkey PRIMARY KEY (id)
);
