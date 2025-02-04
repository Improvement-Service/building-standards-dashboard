import requests
from requests.auth import HTTPBasicAuth
import json
import pandas as pd
import configparser
import logging

logging.basicConfig(level=logging.DEBUG)

if __name__ == "__main__":

    logging.info('Process started')

    ### SETUP ###
    config = configparser.ConfigParser()
    config.read('config.ini')

    api_key = config['DEFAULT']['api_key']
    api_secret_key = config['DEFAULT']['api_secret_key']
    survey_id = config['DEFAULT']['survey_id']
    survey_name = 'Shiny App Export'
    base_url = "https://api.smartsurvey.io/v2/surveys"
    ### END ###

    logging.info('Trying to fetch list of surveys')

    resp = requests.get(
        url = f'{base_url}/{survey_id}/exports',
        auth = HTTPBasicAuth(api_key, api_secret_key)
    )
    if resp.status_code != 200:
        raise Exception(f'ERROR: failed to get survey export list from smart survey. status code: {resp.status_code}')

    survey_export_list = json.loads(resp.content)
    if len(survey_export_list) < 1:
        raise Exception(f'ERROR: No surveys with id:{survey_id} found')

    logging.info(f'Got {len(survey_export_list)} surveys')

    filtered_list = [x for x in survey_export_list if x.'name' == survey_name]
    if len(filtered_list) < 1:
        raise Exception(f'ERROR: No surveys with export name {survey_name} found')

    logging.info(f'Got {len(filtered_list)} surveys with name matching {survey_name}')

    sorted_list = sorted(filtered_list, key=lambda d: d.get('date_started'), reverse=True)

    logging.info(f'Most recent survey is {sorted_list[0]["date_started"]}')

    download_complete = False
    for survey_export in sorted_list:
        if survey_export.get('status') == 'completed':
            resp = requests.get(
                url=survey_export.get('href_download'),
                auth=HTTPBasicAuth(api_key, api_secret_key))
            if resp.status_code != 200:
                raise Exception(f'ERROR: failed to get survey export id:{ survey_export.get("id", "unkown") } from smart survey. status code: {resp.status_code}')

            logging.info(f"Downloading survey completed on {survey_export.get('date_started', 'unknown')}")

            f = open("survey_data.csv", "wb+")
            f.write(resp.content)
            f.close()
            download_complete = True
            break

    if download_complete:
        
        ### TRANSFORM DATA ###
        try:
            logging.info("transforming data")
#             dataframe = pd.read_csv("survey_data.csv")
#             dataframe.to_csv("survey_data.csv")
        except Exception as e:
            logging.warning(e)
        ### END ###
    
        logging.info('survey_data.csv has been successfully downloaded')
    else:
        raise Exception('ERROR: failed to get survey data')
