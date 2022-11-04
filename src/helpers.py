import re
from pathlib import Path

from jsonlines import jsonlines
import pandas as pd

ROOT_DIR = Path("..")
DIR_DAT = ROOT_DIR / 'dat'
DIR_OUTPUT = ROOT_DIR / 'output'

fname = DIR_DAT / 'survey_2022_12_06.csv'

df = pd.read_csv(fname, skiprows = [0,2])

# df = add_other_as_choice_all(df)

# lets deal with those columns separately
df_expect_time_code = pd.concat([
    df.loc[:, df.columns.str.contains("What is your \w+ advisor's", regex=True)],
    df.loc[:, df.columns.str.contains("What is your advisor's", regex=True)],
    df.loc[:, df.columns.str.contains("For any of your current projects,, do you think you spend more time than your", regex=True)]
], axis=1)

df = df.drop(df_expect_time_code.columns.to_list(), axis=1)
old_colnames = df.columns.to_list()
new_colnames = ['start_survey', 'end_survey', 'response_type', 'progress', 'duration_sec', 'is_finished',
                'record_date', 'response_id', 'distribution_channel', 'user_lang', 'captcha_score',
                'agree_term', 'academia_status', 'nb_advisors', 'dept_prof', 'dept_students', 'nb_advisors_2', 
                'first_line_code', 'years_coding', 'self_id_as_coder', 'read_prog_book', 'freq_oss_proj',
                'what_os', 'time_cleaning_code', 'time_data_clean_prog', 
                'time_data_clean_gui', 'time_exp_manip',
                'time_field_data_coll', 'time_grant_writing', 'time_lit_review', 'time_meeting', 'time_read_doc',
                'time_digital_data_coll', 'time_paper_writing', 'self_expect_time_coding', 
                'value_comp_skills_wrt_domain',
                'more_time_learning_to_code', 'pct_social_contacts_coding', 
                'comp_skills_factors_pursue_academia', 'comp_skills_pro_benefits_s',
                'comp_skills_pro_benefits_p', 'comp_skills_recruiting', 'comp_skills_recruiting_undergrad', 'comp_skills_recruiting_grad',
                'comp_skills_recruiting_postdoc', 'cite_code', 'cite_data', 'value_oss_license', 
                'value_coc', 'value_contrib_guide', 'value_cla', 'value_active', 'value_responsive_maintainers',
                'value_welcoming_community', 'value_widespread_use', 'disadv_not_coding', 'coding_on_future_opportunities', 'value_share_code',
                'value_accessibility_paper_code', 'value_paper_code_citability', 'value_learn_code_in_field', 'year_born',
                'country_origin', 'us_state', 'do_del', 'comments', 'score', 'email', 'name_research_group', 'reason_coding',
                'how_did_you_learn_code', 'position_industry', 'freq_coding_proj', 'use_lang', 'enough_instit_support', 'friends_help',
                'perceived_benefits_coding', 'do_share_code_online', 'qualities_oss', 'why_not_coding', 'gender', 
                'underrep_group', 'ethnicity']

with jsonlines.open(DIR_OUTPUT / 'lookup_question.jsonl', 'w') as writer:
    writer.write({ oc: nc for oc, nc in zip(old_colnames, new_colnames) })

