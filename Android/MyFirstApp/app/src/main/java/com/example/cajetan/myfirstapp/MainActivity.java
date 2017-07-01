package com.example.cajetan.myfirstapp;

import android.content.Intent;
import android.os.Build;
import android.support.v4.app.FragmentTransaction;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.view.View;
import android.widget.EditText;

public class MainActivity extends AppCompatActivity
    implements HeadlinesFragment.OnHeadlineSelectedListener {
    public static final String EXTRA_MESSAGE = "com.cajetan.myfirstapp.MESSAGE";

    public void onArticleSelected(int position) {
        // The user selected the headline of an article from the HeadlinesFragment
        // Do something here to display it

        ArticleFragment articleFragment = (ArticleFragment) getSupportFragmentManager().
                findFragmentById(R.id.article_fragment);

        if (articleFragment != null) {
            // articleFragment.updateArticleView(position);
        } else {
            // Otherwise, we're in the one-pane layout and must swap frags...

            // Create fragment and give it an argument for the selected article
            ArticleFragment newFragment = new ArticleFragment();
            Bundle args = new Bundle();
            // args.putInt(ArticleFragment.ARG_POSITION, position);
            newFragment.setArguments(args);

            FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();

            // Replace whatever is in the fragment_container view with this fragment,
            // and add the transaction to the back stack so the user can navigate back
            transaction.replace(R.id.fragment_container, newFragment);
            transaction.addToBackStack(null);

            // Commit the transaction
            transaction.commit();
        }

    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.news_articles);

        // check that the activity is using the layout version with
        // the fragment container FrameLayout
        if (findViewById(R.id.fragment_container) != null) {

            // If we're being restored from a previous state,
            // then we don't need to do anything and should return
            // or else we could end up with overlapping fragments
            if (savedInstanceState != null)
                return;

            // create a new fragment to be placed in the activity layout
            HeadlinesFragment firstFragment = new HeadlinesFragment();

            // in case this activity was started with special instructions from an intent,
            // pass the intent's extras to the fragment as arguments
            firstFragment.setArguments(getIntent().getExtras());

            // add the fragment to the 'fragment_container' FrameLayout
            getSupportFragmentManager().beginTransaction()
                    .add(R.id.fragment_container, firstFragment).commit();

            // create a fragment and give it an argument specifying the aricle it should show
            ArticleFragment newFragment = new ArticleFragment();
            Bundle args = new Bundle();
            // args.putInt(HeadlinesFragment.ARG_POSITION, position);
            newFragment.setArguments(args);

            FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();

            // replace whatever is in the fragment_container view with this fragment,
            // and add the transaction to the back stack so the user can navigate back
            transaction.replace(R.id.fragment_container, newFragment);
            transaction.addToBackStack(null);

            transaction.commit();
        }
    }

    /** Called when the user taps the send button */
    public void sendMessage(View view) {
        Intent intent = new Intent(this, DisplayMessageActivity.class);
        EditText editText = (EditText) findViewById(R.id.editText);
        String message = editText.getText().toString();
        intent.putExtra(EXTRA_MESSAGE, message);
        startActivity(intent);
    }
}
