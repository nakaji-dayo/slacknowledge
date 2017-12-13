create table team (
  id INT NOT NULL AUTO_INCREMENT,
  team_id VARCHAR(31) NOT NULL,
  team_name VARCHAR(255) NOT NULL,
  access_token VARCHAR(255) NOT NULL,
  bot_access_token VARCHAR(255) NOT NULL,
  PRIMARY KEY (`id`)
)
ENGINE InnoDB;
