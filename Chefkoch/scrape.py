from bs4 import BeautifulSoup
import requests
from csv import writer
import json


def get_page(html):
    # TODO: catch request errors
    recipe_page = requests.get(html)
    recipe_page = BeautifulSoup(recipe_page.text, 'html.parser')
    return recipe_page.find("body").find("main")


def get_time_difficulty_date(page):
    prep_time = page[0].text.split('\n')[1].replace(' ', '')
    diff = page[1].text.split('\n')[1].replace(' ', '')
    date = page[2].text.split('\n')[1].replace(' ', '')
    return prep_time, diff, date


def get_tags(page):
    return [tag.text.split('\n')[2].replace(' ', '') for tag in page.find_all("div")]


def get_rating_comments(page):
    title = page.find("h1").get_text()
    avg_rating = float(page.find("div", {"class": "ds-rating-avg"}).get_text().split('\n')[2])
    nr_rating = int(page.find("div", {"class": "ds-rating-count"}).get_text().split('\n')[2])
    nr_comment = int(page.find("span").text.split(' ')[0])
    text = page.find("p").text.split('\n')[1].replace('  ', '')
    return title, avg_rating, nr_rating, nr_comment, text


def get_meta_information(page):

    meta_info = page.find("article").find("div", {"class": "ds-mb-right"})
    tag_info = page.find("amp-carousel")
    time_info = meta_info.find("small").find_all("span")

    title, avg_rating, nr_ratings, nr_comments, text = get_rating_comments(meta_info)
    prep_time, diff, date = get_time_difficulty_date(time_info)
    tags = get_tags(tag_info)

    return title, avg_rating, nr_ratings, prep_time, diff, date, nr_comments, text, tags


def get_recipe_ingredients(page):
    info = page.find_all("article")[1]
    servings = int(info.find("div").find("input")['value'])
    ings = info[1].find_all("table")
    ingredients = []

    for ing in ings:
        ings_add = ing.text.replace(' \n\n', '').replace(' ', '').replace('\n\n\n', '\n').replace('\n\n', '\n').split('\n')
        ings_add = [i for i in ings_add if i != '']
        ingredients += ings_add

    return servings, ingredients


def process_time(time):
    hours = 0
    minutes = 0
    time = time.replace('Stunden', 'Stunde')
    time = time.replace('Stunde', 'Stunden')
    time = time.replace('Minuten', 'Minute')
    time = time.replace('Minute', 'Minuten')

    if 'Stunden' in time and 'Minuten' in time:
        hours = int(time.split('Stunden')[0])
        minutes = int(time.split('Stunden')[1].split('Minuten')[0]
                      )
    if 'Stunden' in time:
        hours = int(time.split('Stunden')[0])

    else:
        minutes = int(time.split('Minuten')[0])

    return 60*hours + minutes


def get_working_time(page):
    info = page.find_all("article")[2].find("small").find_all("span")
    working = info[0].text.split('\n')[1].replace(' ', '').split('.')[1]
    cooking = info[1].text.split('\n')[1].replace(' ', '').split('.')[1]
    total = info[2].text.split('\n')[1].replace(' ', '').split('.')[1]
    return process_time(working), process_time(cooking), process_time(total)




page = requests.get('https://www.chefkoch.de/rs/s0/Rezepte.html') # change s0 accordingly
soup = BeautifulSoup(page.text, 'html.parser')

site = soup.find_all("script")[1]
recipe_urls = json.loads(site.get_text())

recipes = recipe_urls['itemListElement']

rec = recipes[1]['url']
rec_page = get_page(rec)

get_meta_information(rec_page)
serv, ing = get_recipe_ingredients(rec_page)
ing

times = get_working_time(rec_page)


# Get all recipes
# USE PROXIES OR WAIT OR GET PERMISSION
with open('recipes.csv', 'w', newline='') as csv_file:
    csv_writer = writer(csv_file)
    #Get correct column names!
    #csv_writer.writerow(("Name", "RatingAverage", "RatingCount", "Link"))
    for recipe in recipes:
        html = recipe['url']
        page = get_page(html=html)
        meta = get_meta_information(page=page)
        csv_writer.writerow(meta)