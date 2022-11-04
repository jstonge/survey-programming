import re
from pathlib import Path

import numpy as np
import pandas as pd

# pd.options.display.max_columns = 999

ROOT_DIR = Path("..")
DIR_DAT = ROOT_DIR / 'dat'
DIR_OUTPUT = ROOT_DIR / 'output'

fname = DIR_DAT / 'survey_2022_08_12.csv'


def add_other_as_choice(df, main_col, other_col):
    new_col = re.findall("^.+?\?", main_col)[0]
    df[new_col] = np.where(df[main_col] == 'Other', df[other_col], df[main_col])
    df = df.drop([main_col, other_col], axis=1)
    return df

def add_other_as_choice_all(df):
    selected_cols = df.loc[:, df.columns.str.contains(' - Selected Choice', case=True, regex=True)].columns
    other_cols = df.loc[:, df.columns.map(lambda x: bool(re.search(' - (Other|Fill in the blank:) - Text', x)))].columns
    for cs,co in zip(selected_cols, other_cols):
        df = add_other_as_choice(df, cs, co)
    return df

def clean_time_expectation_cols(df):
    
    df["What is your first advisor's main department?"] = np.where(
            df["What is your first advisor's main department?"].isna(),
            df["What is your advisor's main department?"],
            df["What is your first advisor's main department?"]
    )

    df["What is your first advisor's main department?"] = np.where(
        df["What is your first advisor's main department?"].isna(),
        df["What is your advisor's first main department?"],
        df["What is your first advisor's main department?"]
    )

    df['For any of your current projects,, do you think you spend more time than your supervisor expect on programming?'] = np.where(
        df["For any of your current projects,, do you think you spend more time than your supervisor expect on programming?"].isna(),
        df["For any of your current projects,, do you think you spend more time than your first supervisor expect on programming?"],
        df["For any of your current projects,, do you think you spend more time than your supervisor expect on programming?"]
    )

    df['For any of your current projects,, do you think you spend more time than your supervisor expect on programming?'] = np.where(
        df["For any of your current projects,, do you think you spend more time than your supervisor expect on programming?"].isna(),
        df["For any of your current projects,, do you think you spend more time than your first supervisor expect on programming?.1"],
        df["For any of your current projects,, do you think you spend more time than your supervisor expect on programming?"]
    )

    df["What is your second advisor's main department?"] = np.where(
            df["What is your second advisor's main department?"].isna(),
            df["What is your second advisor's main department?.1"],
            df["What is your second advisor's main department?"]
    )

    df['For any of your current projects,, do you think you spend more time than your second supervisor expect on programming?'] = np.where(
        df["For any of your current projects,, do you think you spend more time than your second supervisor expect on programming?"].isna(),
        df["For any of your current projects,, do you think you spend more time than your second supervisor expect on programming?.1"],
        df["For any of your current projects,, do you think you spend more time than your second supervisor expect on programming?"]
    )

    # df = df.rename(columns={
        
    # })

    df = df.drop(["What is your advisor's first main department?", "What is your advisor's main department?", "What is your first advisor's main department?",
                  "What is your second advisor's main department?", "What is your second advisor's main department?.1", "For any of your current projects,, do you think you spend more time than your supervisor expect on programming?", "For any of your current projects,, do you think you spend more time than your first supervisor expect on programming?",
                  "For any of your current projects,, do you think you spend more time than your second supervisor expect on programming?", "For any of your current projects,, do you think you spend more time than your first supervisor expect on programming?.1",
                  "For any of your current projects,, do you think you spend more time than your second supervisor expect on programming?.1"], axis=1)
    return df

def clean_colnames(df):
    # deal with duplicated cols (selected vs other choices)
    df = add_other_as_choice_all(df)

    # lets deal with those columns separately
    df_expect_time_code = pd.concat([
        df.loc[:, df.columns.str.contains("What is your \w+ advisor's", regex=True)],
        df.loc[:, df.columns.str.contains("What is your advisor's", regex=True)],
        df.loc[:, df.columns.str.contains("For any of your current projects,, do you think you spend more time than your", regex=True)]
    ], axis=1)
    
    df = df.drop(df_expect_time_code.columns.to_list(), axis=1)

    # clean all other cols
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

    df = df.rename(columns = {
        oc: nc for oc, nc in zip(old_colnames, new_colnames)
    })

    # clean expectation cols
    df_expect_time_code = clean_time_expectation_cols(df_expect_time_code)

    df_expect_time_code = df_expect_time_code.rename(columns={ 
        "What is your first advisor's main department?": "first_adv_dept",
        "For any of your current projects,, do you think you spend more time than your supervisor expect on programming?": "first_adv_expect_time_coding",
        "What is your second advisor's main department?":"second_adv_dept",
        "For any of your current projects,, do you think you spend more time than your second supervisor expect on programming?":"second_adv_expect_time_coding",
        "What is your third advisor's main department?": "third_adv_dept",
        "For any of your current projects,, do you think you spend more time than your third supervisor expect on programming?": "third_adv_expect_time_coding"
    })

    df_expect_time_code = df_expect_time_code.drop(
        ["What is your second advisor's main department?.1",
         "For any of your current projects,, do you think you spend more time than your first supervisor expect on programming?"
         "For any of your current projects,, do you think you spend more time than your first supervisor expect on programming?.1"
         "For any of your current projects,, do you think you spend more time than your second supervisor expect on programming?.1"
        ], axis=1)
    # put back together
    df = pd.concat([df, df_expect_time_code], axis=1)
    
    return df
    

df = pd.read_csv(fname, skiprows = [0,2])

df = clean_colnames(df)

# cleaning values
df['is_coder'] = np.where(df['reason_coding'] != 'I  do not know how to code', 1, 0)
df['year_born'] = pd.to_datetime(df['year_born'], format="%Y").dt.year

df.to_csv(DIR_OUTPUT / 'data_clean.csv', index=False)


