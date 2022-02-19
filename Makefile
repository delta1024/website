# Copyright (C) Jacob Stannix

# This file is part of jakestannix.org.

# This file is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 3 of
# the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public
# License along with this file. If not, see
# https://www.gnu.org/licenses/.

####### General Variables #######
HOST = root@jakestannix.org:/var/www/personal
BLOG_DIR := blog
BLOG_POST_DIR := $(BLOG_DIR)/posts
CSS_DIR := styles

####### Output Directories #######
OUTPUT_DIR = site-dir
BLOG_OUTPUT_DIR := $(OUTPUT_DIR)/$(BLOG_DIR)
CSS_OUTPUT_DIR := $(OUTPUT_DIR)/$(CSS_DIR)
BLOG_POST_OUTPUT_DIR := $(OUTPUT_DIR)/$(BLOG_POST_DIR)

####### Command Variables #######
RSYNC_COMMAND = rsync -vrP --delete-after $(OUTPUT_DIR)/ $(HOST)
BUILD_COMMAND = emacs -q -Q -nw --script ./publish.el --generate-section

####### Source Files #######
base_css_files := $(wildcard $(CSS_DIR)/*.css)
blog_css_files := $(wildcard $(BLOG_DIR)/posts/styles/*.css)
blog_posts := $(wildcard $(BLOG_DIR)/posts/*.org)
index_files := index.org blog/index.org

####### Output Files #######
blog_post_output := $(addprefix $(OUTPUT_DIR)/,$(blog_posts:.org=.html))
base_css_output := $(addprefix $(OUTPUT_DIR)/,$(base_css_files))
blog_css_output := $(addprefix $(OUTPUT_DIR)/,$(blog_css_files))
index_files_output := $(addprefix $(OUTPUT_DIR)/,$(index_files:.org=.html))

####### Phony Rules #######

.PHONY: all clean sync index css post quick

all: $(index_files_output) $(base_css_output) $(blog_post_output) $(blog_css_output) 

index:
	$(BUILD_COMMAND) website

css:
	$(BUILD_COMMAND) css

post:
	$(BUILD_COMMAND) blog

sync: 		
	$(MAKE) fast
	$(RSYNC_COMMAND)

clean:
	rm -rf $(OUTPUT_DIR)

quick:
	$(BUILD_COMMAND) all

####### File Rules #######

$(OUTPUT_DIR) $(BLOG_OUTPUT_DIR) $(CSS_OUTPUT_DIR) $(BLOG_POST_OUTPUT_DIR):
	mkdir $@

$(index_files_output): $(index_files) | $(OUTPUT_DIR) $(BLOG_OUTPUT_DIR)
	$(BUILD_COMMAND) website

$(base_css_output) $(blog_css_output): $(base_css_files) $(blog_css_files)| $(CSS_OUTPUT_DIR)
	$(BUILD_COMMAND) css

$(blog_post_output): $(blog_posts) | $(BLOG_POST_OUTPUT_DIR) 
	$(BUILD_COMMAND) blog

