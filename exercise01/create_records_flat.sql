create table records_flat as 
select records.*, 
       countries.name as native_country,
       education_levels.name as highest_education_lvl,
       marital_statuses.name as marital_status,
       occupations.name as occupation,
       races.name as race,
       relationships.name as relationship,
       sexes.name as gender,
       workclasses.name as working_class
from records left join countries on records.country_id = countries.id
             left join education_levels on records.education_level_id = education_levels.id
             left join marital_statuses on records.marital_status_id = marital_statuses.id 
             left join occupations on records.occupation_id = occupations.id
             left join races on records.race_id = races.id
             left join relationships on records.relationship_id = relationships.id
             left join sexes on records.sex_id = sexes.id
             left join workclasses on records.workclass_id = workclasses.id;
